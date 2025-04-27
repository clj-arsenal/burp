(ns test
  (:require
   [clj-arsenal.check :as check]
   [clj-arsenal.burp]))

(defn run
  [& _]
  (check/report-all-checks-and-exit!))
