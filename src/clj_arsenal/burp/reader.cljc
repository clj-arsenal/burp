(ns clj-arsenal.burp.reader
  (:require
   [clojure.string :as str]
   [clj-arsenal.burp.impl :refer [->BurpElementKey ->BurpElement]]))

(defonce !cache (atom {}))

(defn- normalize-element-node
  [[operator & others :as form]]
  (if-not (or (symbol? operator) (keyword? operator))
    (throw (ex-info ))))

(defn- burpify
  [form]
  (cond
    (vector? form)
    (let [[operator props body] (normalize-element-node form)]
      (cond
        (keyword? operator)
        (let [parsed-tag-components (parse-tag operator)]
          (reduce
            (fn [inner-burp-element [component-tag component-props]]
              (->BurpElement
                (->BurpElementKey component-tag nil)
                component-props
                [inner-burp-element]))
            (let [[innermost-tag innermost-props] (peek parsed-tag-components)]
              (->BurpElement
                (->BurpElementKey innermost-tag (::key (meta form)))
                (cond-> props
                  (not (str/blank? (:className innermost-tag)))
                  (assoc :className
                    (if (str/blank? (:className props))
                      (:className innermost-tag)
                      (str (:className innermost-tag) " " (:className props))))
                  
                  (and (:id innermost-props) (not (:id props)))
                  (assoc :id (:id innermost-props)))
                (vec body)))
            (reverse (subvec parsed-tag-components 0 (dec (count parsed-tag-components))))))))))

(defmacro foo [x]
  `(with-meta ~x ~(meta x)))

(macroexpand `(foo ^{:key 123} [:foo "blah"]))
