(defproject autobase "0.1.0-SNAPSHOT"
  :description "Generate crud sources for a web app."
  :url "https://github.com/gpsoft/autobase.git"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [de.ubercode.clostache/clostache "1.4.0"]
                 [camel-snake-kebab "0.4.0"]
                 [inflections "0.13.0"]]
  :main ^:skip-aot autobase.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
