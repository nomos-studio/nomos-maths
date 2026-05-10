; SPDX-License-Identifier: EPL-2.0
(defproject cljseq-maths "0.1.0"
  :description "cljseq-maths — pure signal and DSP mathematics for the cljseq ecosystem"
  :url "https://github.com/cljseq/cljseq-maths"
  :license {:name "EPL-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.12.2"]]
  :source-paths ["src"]
  :test-paths   ["test"]
  :target-path  "target/%s"
  :profiles {:dev {:dependencies []}})
