(ns objclj.core
  (:use clojure.walk))

(comment
  EXAMPLE
  (c
   (defn int main
     [int argc, char** argv]
     (printf "%i args" argc)
     (return 1)))
  )

(def *sep* ";\n")

(def *infix-ops* (quote #{+ - * / % < > && || << >> | +=}))

(defn join
  [l x]
  (reduce #(str %1 x %2) l))

; this is the fundamental form for printing C code
(defmacro c
  [code]
  `(emit '~code))

; emit returns a string represeting C code
(defmulti emit type)

; emit-special handles special forms
(defmulti emit-special (fn [& args] (-> args first identity)))

; the following var and macro are for setting up the special forms
; see defspecial examples below
(def *special-forms* #{})
(defmacro defspecial
  [f & body]
  `(do
     (def *special-forms* (conj *special-forms* '~f))
     (defmethod emit-special '~f ~@body)))

(defmethod emit :default [expr] (pr-str expr))

(defn emit-objc-send
  [sel-keyword target args]
  (format "[%s %s]" target (name sel-keyword)))

(defmethod emit clojure.lang.PersistentList
  ; the core of it; emits C code for list forms
  ; handles objc sends, special forms, infix and function calls
  [[f & args :as expr]]
  (cond
   (keyword? f) (emit-objc-send f (first args) (rest args))
   (contains? *special-forms* f) (apply emit-special f args)
   (contains? *infix-ops* f) (format "%s %s %s" (emit (first args)) f (emit (second args)))
   :else (format "%s(%s)" (emit f) (join (map emit args) ", "))))

(defmethod emit clojure.lang.PersistentVector
  [v]
  ; this is going to be tricky...
  (format "{ %s }" (join (map emit v) ", ")))

(defmethod emit clojure.lang.Named
  [x]
  (name x))

(defmethod emit clojure.lang.Ratio
  [r]
  (str (float r)))

(defspecial .
  [_ target refinement]
  (format "%s.%s" target refinement))

(defspecial code
  ; just return the literal string as code
  [_ expr]
  (str expr))

(defspecial var
  ; a declaration (int x)
  ([_ type- lval]
     (format "%s %s" (emit type-) (emit lval)))
  ([_ type- lval rval]
     (format "%s %s = %s" (emit type-) (emit lval) (emit rval))))

(defspecial set!
  ; assignment: lval = rval
  [_ lval rval]
  (format "%s = %s" (emit lval) (emit rval)))

(defspecial ref
  [_ expr]
  (format "&(%s)" (emit expr)))

(defspecial deref
  [_ expr]
  (format "*(%s)" (emit expr)))

(defspecial do
  [_ & body]
  (format "{ %s }" (apply str (map #(str (emit %) *sep*) body))))

(defspecial return
  [_ expr]
  (format "return %s" (emit expr)))

(defspecial inc
  [_ x]
  (format "(%s + 1)" (emit x)))

(defspecial dec
  [_ x]
  (format "(%s - 1)" (emit x)))

(defspecial if
  [_ case true-code false-code]
  (format "if (%s) %s; else %s;" (emit case) (emit true-code) (emit false-code)))

(defspecial =
  [_ l r]
  (format "(%s == %s)" (emit l) (emit r)))

(defn c-bindings
  [bindings]
  (assert (vector? bindings))
  (apply str
	 (map
	  (fn [[type- lval rval]]
	    (str (emit (list 'var type- lval rval)) *sep*))
	  (partition 3 bindings))))

(defspecial let
  [_ bindings & exprs]
  (assert (vector? bindings))
  (format "{ %s %s }"
	  (c-bindings bindings)
	  (apply str (map #(str (emit %) *sep*) exprs))))

(defspecial cast
  [_ type- x]
  (format "((%s)(%s))" (emit type-) (emit x)))

(defspecial defn
  [_ ret-type fn-name arglist & body]
  (assert (vector? arglist))
  (format "%s %s(%s) { %s }"
	  (emit ret-type)
	  (emit fn-name)
	  (join (map (fn [[arg-type arg]] (str (emit arg-type) " " (emit arg))) (partition 2 arglist)) ", ")
	  (apply str (map #(str (emit %) *sep*) body))))

(defspecial nth
  [_ ptr offset]
  (format "(%s)[%s]" (emit ptr) (emit offset)))