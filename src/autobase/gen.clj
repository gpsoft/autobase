(ns autobase.gen
  (:require [clostache.parser :as p]))

(defn gen-code
  [ent]
  )

(comment
  (p/render "Hello, {{name}}!" {:name "Felix"})
  (p/render-resource "hoge.edn" {:name "Felix"})
  )
