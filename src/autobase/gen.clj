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

(declare text-renderer select-renderer radios-renderer)
(def ^:private edit-map
  {:text {:prefix "txt" :renderer text-renderer}
   :textarea {:prefix "txa"}
   :select {:prefix "sel" :renderer select-renderer}
   :radio {:prefix "rad" :renderer radios-renderer}
   :check {:prefix "chk"}})

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

(defn- hungarian-prefix
  [t]
  (let [k (keyword t)]
    (get-in type-map [k :prefix])))

(defn- edit-prefix
  [opts]
  (let [k (edit-type opts)]
    (get-in edit-map [k :prefix])))

(defn- edit-renderer
  [opts]
  (let [k (edit-type opts)]
    (get-in edit-map [k :renderer])))

(defn- options-array
  [opts]
  (if-let [s (:foreign opts)]
    (array-name s)
    (when-let [s (:code-master opts)]
      (array-name s))))

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
   (let [prefix (edit-prefix opts)
         prefix (str prefix (if search? "Search" ""))]
     (camel-case cname prefix))))

(defn- array-name
  [n]
  (-> n
      plural
      (camel-case "ary")))

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

(defn- inp-class-invalid
  [[t opts]]
  (let [et (edit-type opts)
        datetime? (some? (:datetime opts))]
    (when (and (= et :text)
             (not datetime?))
      "invalid")))
(defn- inp-class-date
  [[t opts]]
  (let [date? (= (:datetime opts) :date)]
    (when date?
      "date")))
(defn- inp-classes
  [t opts]
  (let [fs (juxt inp-class-invalid
                 inp-class-date)
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

(defn- text-renderer
  [pname classes opts]
  (fcall-expr
    "outText"
    [(wrapsq pname)
     (wrapsq classes)]))
(defn- select-renderer
  [pname classes opts]
  (let [a (options-array opts)]
    (fcall-expr
      "outSelect"
      [(wrapsq pname)
       (variable a)
       (wrapsq classes)])))
(defn- radios-renderer
  [pname classes opts]
  (let [a (options-array opts)]
    (fcall-expr
      "outRadios"
      [(wrapsq pname)
       (variable a)
       (wrapsq classes)])))
(defn- edit-expr
  [cname t opts]
  (let [pname (param-name cname opts)
        classes (inp-classes t opts)
        renderer (edit-renderer opts)]
    (when renderer
      (renderer pname classes opts))))

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
      (edit-expr cname t opts)
      " ?></td>\n")))

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
     :edit-section #(edit-section prop %)}))

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
