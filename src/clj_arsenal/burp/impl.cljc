(ns ^:no-doc clj-arsenal.burp.impl
  (:import
     #?@(:cljd []
         :clj [(java.util HashMap)])))

(defrecord BurpElementKey [operator custom-key])
(defrecord BurpElement [key props body])

(defn element?
  [x]
  (instance? BurpElement x))

(defn element-key?
  [x]
  (instance? BurpElementKey x))

(defn -flatten-body
  [body]
  (cond
    (nil? body) nil
    (seq? body) (mapcat -flatten-body body)
    :else (list body)))
