; SPDX-License-Identifier: EPL-2.0
(defproject nomos-maths "0.2.0"
  :description "nomos-maths — pure signal and DSP mathematics for the nomos-studio ecosystem"
  :url "https://github.com/nomos-studio/nomos-maths"
  :license {:name "EPL-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.12.2"]]
  :source-paths ["src"]
  :test-paths   ["test"]
  :target-path  "target/%s"
  :profiles {:dev {:dependencies []}})
