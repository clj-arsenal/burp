(ns ^:no-doc clj-arsenal.burp.impl
  (:import
     #?@(:cljd []
         :clj [java.util.HashMap])))

(defrecord BurpElementKey [tag custom-key])
(defrecord BurpElement [key props body])

(def ^:private once-cache #?(:cljs #js{} :cljd (Map) :clj (java.util/HashMap.)))

(defn once-cache-get
  [k]
  #?(:cljs (aget once-cache k) :cljd (aget once-cache k) :clj (.get once-cache k)))

(defn once-cache-put!
  [k v]
  #?(:cljs (aset once-cache k v) :cljd (aset once-cache k v) :clj (.put once-cache k v))
  v)

(defn element?
  [x]
  (instance? BurpElement x))

(defn element-key?
  [x]
  (instance? BurpElementKey x))
