(ns autobase.core
  (:require [autobase.gen :refer :all])
  (:gen-class))

(def ^:private load-edn
  (comp clojure.edn/read-string slurp))

(defn- show-usage []
  (println "Usage: autobase ENTITY_DESC_FILE")
  (println "   ex: autobase user.edn"))

(defn -main
  [& args]
  (let [[ent-file] args]
    (when (nil? ent-file)
      (show-usage)
      (System/exit -1))
    (let [ent (load-edn ent-file)]
      (gen-code ent))))

(comment
  (-> "hoge.edn"
      load-edn
      gen-code))
