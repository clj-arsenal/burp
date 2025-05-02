> [!WARNING]
> Unstable/WIP; avoid using.

A library to produce markup/component trees. Designed
for use with `clj-arsenal/wc`, `clj-arsenal/vdom`, and
`clj-arsenal/html`.

```clojure
(require '[clj-arsenal.burp :refer [$]])

(list
 ($ :div#foo.my-class
   {:style {:display :flex}}
   "Foo")
 ($ :div#bar
  "Bar"))
```
