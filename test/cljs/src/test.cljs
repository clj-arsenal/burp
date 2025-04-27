(ns test
  (:require
   [clj-arsenal.check :as check]
   [clj-arsenal.burp]))

(defn run
  []
  (check/report-all-checks-and-exit!))
