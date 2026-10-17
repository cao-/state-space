(ns build
  (:require [clojure.tools.build.api :as b]))

(def lib 'org.clojars.cao/state-space)
(def class-dir "target/classes")
(def jar-file "target/state-space.jar")
(def basis (delay (b/create-basis {:project "deps.edn"})))

(defn jar [{:keys [version]}]
  (let [version (or version (System/getenv "STATE_SPACE_VERSION"))]
    (when-not (and (string? version)
                   (re-matches #"\d+\.\d+\.\d+" version))
      (throw (ex-info "Version must have the form MAJOR.MINOR.PATCH."
                      {:version version})))
    (b/delete {:path class-dir})
    (b/copy-dir {:src-dirs ["src"]
                 :target-dir class-dir})
    (b/write-pom {:class-dir class-dir
                  :lib lib
                  :version version
                  :basis @basis
                  :src-dirs ["src"]
                  :scm {:url "https://github.com/cao-/state-space"
                        :connection "scm:git:git://github.com/cao-/state-space.git"
                        :developerConnection "scm:git:ssh://git@github.com/cao-/state-space.git"
                        :tag (str "v" version)}
                  :pom-data [[:description "State-space exploration for Clojure."]
                             [:url "https://github.com/cao-/state-space"]
                             [:licenses
                              [:license
                               [:name "MIT License"]
                               [:url "https://opensource.org/license/mit/"]
                               [:distribution "repo"]]]]})
    (b/jar {:class-dir class-dir
            :jar-file jar-file})))
