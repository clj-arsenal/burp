(ns ^:no-doc clj-arsenal.burp.impl
  (:import
     #?@(:cljd []
         :clj [(java.util HashMap)])))

(defrecord BurpElementKey [tag custom-key])
(defrecord BurpElement [key props body])

(defonce ^:private once-cache #?(:cljs (js/Map.) :cljd (Map) :clj (HashMap.)))

(defn once-cache-get
  [k]
  #?(:cljs (.get once-cache k) :cljd (aget once-cache k) :clj (.get once-cache k)))

(defn once-cache-put!
  [k v]
  #?(:cljs (.set once-cache k v) :cljd (aset once-cache k v) :clj (.put once-cache k v))
  v)

(defn element?
  [x]
  (instance? BurpElement x))

(defn element-key?
  [x]
  (instance? BurpElementKey x))
