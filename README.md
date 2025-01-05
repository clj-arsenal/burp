A hiccup-like markup library that normalizes forms
and extracts constants during macro-expansion. Designed
for use with `clj-arsenal/wc`, `clj-arsenal/vdom`, and
`clj-arsenal/html`.

```clojure
(require '[clj-arsenal.burp :refer [burp]])

(burp
 [:div#foo.my-class
   {:style {:display :flex}}
   "Foo"]
 [:div#bar
  "Bar"])
```

Unfortunately the Clojure reader doesn't attach source location
information to non-list forms; so if you wan't better location
info in error reports, this alternate form can be used.

```clojure
(require '[clj-arsenal.burp :refer [$]])

($ :div#foo.myclass
  {:style {:display :flex}}
  "Foo")
```

Burp doesn't support inline nesting tags with a `>` as some
hiccup-like notations allow (e.g `:div>span`), because burp
(and the libraries that use burp) allow namespace qualified
keywords as tags, which doesn't work so well with this
notation.  Instead use vectors for inline nesting: `[:div :span]`.
