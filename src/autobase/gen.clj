(ns autobase.gen
  (:require [clostache.parser :as p]
            [camel-snake-kebab.core :refer [->PascalCase]]))

;; for debugging
(require 'clojure.pprint) ;; need this?
(defn- tap [v] (clojure.pprint/pprint v) v)

(def ^:private type-map
  {:str {:prefix "str" :empty-val "''"}
   :num {:prefix "int" :empty-val 0}
   :int {:prefix "int" :empty-val 0}
   :boolean {:prefix "bln" :empty-val false}
   :array {:prefix "ary" :empty-val "Array()"}
   :mix {:prefix "mix" :empty-val "null"}
   :class {:prefix "cls" :empty-val "null"}
   :object {:prefix "obj" :empty-val "null"}})

(def ^:private edit-map
  {:text {:prefix "txt"}
   :textarea {:prefix "txa"}
   :select {:prefix "sel"}
   :radio {:prefix "rad"}
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

(defn- pascal-case
  [cname prefix]
  (let [n (->PascalCase cname)]
    (str prefix n)))

(defn- var-name
  [cname t]
  (pascal-case cname (hungarian-prefix t)))

(defn- hid-name
  [cname]
  (pascal-case cname "hid"))

(defn- param-name
  [cname opts]
  (pascal-case cname (edit-prefix opts)))

(defn- empty-val
  [t opts]
  (when-not (:required? opts)
    (let [emp-val (:empty opts)
          k (keyword t)]
      (if emp-val
        emp-val
        (get-in type-map [k :empty-val])))))

(defn- table-name
  [p t]
  (str p "_" t))

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

(defn- inp-classes
  [opts]
  (enc-str ["abc" "good"]))

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

(defn- edit-expr
  [cname opts]
  (let [pname (param-name cname opts)
        classes (inp-classes opts)]
    (fcall-expr
      "fuga"
      [(wrapsq pname)
       (wrapsq classes)])))

(defn- edit-section
  [prop]
  (let [[pname cname t opts] prop
        required? (:required? opts)]
    (str
      "<? hoge("
      (wrapsq pname)
      (if required? ", true" "")
      "); ?>\n"
      "<td><? "
      (edit-expr cname opts)
      " ?></td>")))

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
     :edit-section (edit-section prop)}))

(defn- data-from-ent
  [ent]
  (let [gin #(get-in ent %)
        proj-name (gin [:project])
        bs-id (gin [:bs-id])
        detail? (gin [:detail?])
        t (gin [:entity :table])]
    {:proj-name proj-name
     :ent-name (gin [:entity :name])
     :table-name (table-name proj-name t)
     :this-year (this-year)
     :disp-ids (disp-ids bs-id detail?)
     :properties (properties (gin [:entity :props]))
     :detail? detail?
     }))

(defn gen-code
  [ent]
  (let [d (data-from-ent ent)
        f "base.php"
        c (p/render-resource f d)
        o f]
    (spit o c)))

(comment
  (p/render "Hello, {{name}}!" {:name "Felix"})
  (p/render-resource "base.php" {:name "Felix"})
  )
