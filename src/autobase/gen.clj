(ns autobase.gen
  (:require [clostache.parser :as p]
            [camel-snake-kebab.core :refer [->PascalCase]]
            [inflections.core :refer [plural]]))

;; for debugging
(require 'clojure.pprint) ;; need this?
(defn- tap [v] (clojure.pprint/pprint v) v)

(def ^:private templs
  {"partial.php" :php-common
   "base.php" :php-common
   "s00.php" :php
   "s00.js" :js})

(def ^:private type-map
  {:str {:prefix "str" :empty-val "''"}
   :num {:prefix "int" :empty-val 0}
   :int {:prefix "int" :empty-val 0}
   :boolean {:prefix "bln" :empty-val false}
   :array {:prefix "ary" :empty-val "Array()"}
   :mix {:prefix "mix" :empty-val "null"}
   :class {:prefix "cls" :empty-val "null"}
   :object {:prefix "obj" :empty-val "null"}})

(declare text-rdr textarea-rdr select-rdr radios-rdr checks-rdr
         s-text-rdr s-select-rdr s-radios-rdr s-checks-rdr s-date-range-rdr)
(def ^:private edit-map
  {:text {:prefix "txt" :renderer #'text-rdr}
   :textarea {:prefix "txa" :renderer #'textarea-rdr}
   :select {:prefix "sel" :renderer #'select-rdr}
   :radio {:prefix "rad" :renderer #'radios-rdr}
   :check {:prefix "chk" :renderer #'checks-rdr}})
(def ^:private search-map
  (merge-with
    merge edit-map
    {:text {:renderer #'s-text-rdr}
     :textarea {:renderer #'s-text-rdr}
     :select {:renderer #'s-select-rdr}
     :radio {:renderer #'s-radios-rdr}
     :check {:renderer #'s-checks-rdr}
     :date-range {:prefix "txt" :renderer #'s-date-range-rdr}}))

(defn- enc-str
  [items
   & {:keys [sepa empty-ok?]
      :or {sepa " " empty-ok? false}}]
  (let [items (if empty-ok? items (filter some? items))]
    (clojure.string/join sepa items)))
(defn- wrapsq [s] (str \' s \'))
(defn- wrapdq [s] (str \" s \"))
(defn- wrappar [s] (str \( s \)))
(defn- variable [s] (str "$" s))

(defn- edit-type
  [opts] (get opts :edit :text))
(defn- search-type
  [opts] (get opts :search :text))

(defn- hungarian-prefix
  [t]
  (let [k (keyword t)]
    (get-in type-map [k :prefix])))

(defn- type-fn-and-map
  [search?]
  (if search?
    [search-type search-map]
    [edit-type edit-map]))

(defn- param-prefix
  [opts search?]
  (let [[f m] (type-fn-and-map search?)
        k (f opts)]
    (get-in m [k :prefix])))

(defn- ctl-renderer
  [opts search?]
  (let [[f m] (type-fn-and-map search?)
        k (f opts)]
    (get-in m [k :renderer])))

(defn- camel-case
  [cname prefix]
  (when cname
    (let [n (->PascalCase cname)]
      (str prefix n))))

(defn- var-name
  [cname t]
  (camel-case cname (hungarian-prefix t)))

(defn- hid-name
  [cname]
  (camel-case cname "hid"))

(defn- param-name
  ([cname opts] (param-name cname opts false))
  ([cname opts search?]
   (let [prefix (param-prefix opts search?)
         prefix (str prefix (if search? "Search" ""))]
     (camel-case cname prefix))))

(defn- array-name
  [n]
  (-> n
      plural
      (camel-case "ary")))

(defn- options-array
  [opts]
  (if-let [s (:foreign opts)]
    (array-name s)
    (when-let [s (:code-master opts)]
      (array-name s))))

(defn- empty-val
  [t opts]
  (when-not (:required? opts)
    (let [emp-val (:empty opts)
          k (keyword t)]
      (if emp-val
        emp-val
        (get-in type-map [k :empty-val])))))

(defn- table-name
  [p tn]
  (str p "_" tn))

(defn- this-year []
  (.getYear (java.time.LocalDate/now)))

(defn- prop-name-to-col-name
  [prop]
  (let [[pname cname t opts] prop]
    (str (wrapsq pname) " => " (wrapsq cname))))

(defn- col-name-to-var-name
  [prop]
  (let [[pname cname t opts] prop]
    (str (wrapsq cname) " => " (wrapsq (var-name cname t)))))

(defn- col-name-to-param-name
  [prop]
  (let [[pname cname t opts] prop]
    (str (wrapsq cname) " => " (wrapsq (param-name cname opts)))))

(defn- array-expr
  [items]
  (str "Array("
       (enc-str items :sepa ", ")
       ")"))

(defn- upsert-expr
  [cname t opts]
  (let [emp-val (empty-val t opts)
        upsert [(wrapsq cname) (wrapsq (name t)) emp-val]
        upsert (filter some? upsert)]
    (if (and (= (count upsert) 2) (= t :str))
      (first upsert)
      (array-expr upsert))))

(defn- hid-name-to-upsert
  [prop]
  (let [[pname cname t opts] prop]
    (str (wrapsq (hid-name cname))
         " => "
         (upsert-expr cname t opts))))

(defn- ctl-class-invalid
  [[t opts]]
  (let [et (edit-type opts)
        datetime? (some? (:datetime opts))]
    (when (and (= et :text)
             (not datetime?))
      "invalid")))
(defn- ctl-class-date
  [[t opts]]
  (let [date? (= (:datetime opts) :date)]
    (when date?
      "date")))
(defn- ctl-classes
  [t opts]
  (let [fs (juxt ctl-class-invalid
                 ctl-class-date)
        classes (fs [t opts])]
    (enc-str classes)))

(defn- elm-expr
  [tag m]
  (let [prop-exprs (for [[n v] m
                    :let [v (if (coll? v) (enc-str v) v)]]
                (str (name n)
                     "="
                     (wrapdq v)))]
    (str "<" tag " " (enc-str prop-exprs) " />")))

(defn- fcall-expr
  [func params]
  (str func (wrappar (enc-str params :sepa ", "))))

(defn- text-rdr
  [pname para classes opts]
  (fcall-expr
    "outText"
    [(wrapsq para)
     (wrapsq classes)]))
(defn- select-rdr
  [pname para classes opts]
  (let [a (options-array opts)]
    (fcall-expr
      "outSelect"
      [(wrapsq para)
       (variable a)
       (wrapsq classes)])))
(defn- radios-rdr
  [pname para classes opts]
  (let [a (options-array opts)]
    (fcall-expr
      "outRadios"
      [(wrapsq para)
       (variable a)
       (wrapsq classes)])))
(defn- s-text-rdr
  [pname para classes opts]
  (fcall-expr
    "outSearchText"
    [(wrapsq pname)
     (wrapsq para)
     (wrapsq classes)]))
(defn- s-select-rdr
  [pname para classes opts]
  (let [a (options-array opts)]
    (fcall-expr
      "outSearchSelect"
      [(wrapsq pname)
       (wrapsq para)
       (variable a)
       (wrapsq classes)])))
(defn- s-checks-rdr
  [pname para classes opts]
  (let [a (options-array opts)]
    (fcall-expr
      "outSearchChecks"
      [(wrapsq pname)
       (wrapsq para)
       (variable a)
       (wrapsq classes)])))
(defn- s-date-range-rdr
  [pname para classes opts]
  "//TODO: search for date range")

(defn- ctl-expr
  [pname cname t opts e-or-s]
  (let [search? (= e-or-s :search)
        para (param-name cname opts search?)
        classes (ctl-classes t opts)
        rdr (ctl-renderer opts search?)]
    (when rdr
      (rdr pname para classes opts))))

(defn- edit-section
  [prop _]
  (let [[pname cname t opts] prop
        required? (:required? opts)]
    (str
      "<? hoge("
      (wrapsq pname)
      (if required? ", true" "")
      "); ?>\n"
      "<td><? "
      (ctl-expr pname cname t opts :edit)
      "; ?></td>\n")))

(defn- search-section
  [prop _]
  (let [[pname cname t opts] prop]
    (str (ctl-expr pname cname t opts :search) ";" \newline)))

(defn- disp-ids
  [bs-id detail?]
  (for [i (filter some? [\s \u \c \r (if detail? \d nil)])]
    {:disp-id (str bs-id i "00")}))

(defn- properties
  [props]
  (for [prop props]
    {:prop-name-to-col-name (prop-name-to-col-name prop)
     :col-name-to-var-name (col-name-to-var-name prop)
     :col-name-to-param-name (col-name-to-param-name prop)
     :hid-name-to-upsert (hid-name-to-upsert prop)
     ;; ↓ラムダを使う。
     ;;   リテラルを使うと、なぜか $ が \$ と出力されてしまう。
     ;;   clostacheのバグっぽい。
     ;;   また、このときテンプレート側は、implicit iteratorsを使う。
     ;;     {{#edit-section}}
     ;;     {{.}}
     ;;     {{/edit-section}}
     ;;   みたいな感じ。
     :edit-section #(edit-section prop %)
     :search-section #(search-section prop %)}))

(defn- data-from-ent
  [ent]
  (let [gin #(get-in ent %)
        proj-name (gin [:project])
        bs-id (gin [:bs-id])
        detail? (gin [:detail?])
        tn (gin [:entity :table])]
    {:proj-name proj-name
     :bs-id bs-id
     :ent-name (gin [:entity :name])
     :table-name (table-name proj-name tn)
     :this-year (this-year)
     :disp-ids (disp-ids bs-id detail?)
     :properties (properties (gin [:entity :props]))
     :detail? detail?
     }))

(defn- gen-one
  [templ d out]
  (let [code (p/render-resource templ d)]
    (spit out code)))

(defn- out-dir-path
  [ft bs-id]
  (condp = ft
    :php-common bs-id
    :php (str bs-id)
    :js (str "js/" bs-id)))

(defn- out-file-path
  [templ ft bs-id dir]
  (str dir "/" (if (= ft :php-common) "" bs-id) templ))

(defn gen-code
  [ent]
  (let [d (data-from-ent ent)
        bs-id (:bs-id d)]
    (dorun (for [[templ ft] templs
                 :let [dir (out-dir-path ft bs-id)
                       _ (.mkdirs (java.io.File. dir))
                       out (out-file-path templ ft bs-id dir)]]
             (gen-one templ d out)))))

(comment
  (p/render "Hello, {{name}}!" {:name "Felix"})
  (p/render-resource "base.php" {:name "Felix"})
  )
