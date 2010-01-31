(ns se.raek.pyttelisp
 (:refer-clojure :exclude [type eval apply]))

(defn bool? [x]
  (or (true? x)
      (false? x)))

(defn tag [key val]
  (se.raek.Tag. key val))

(defn tag? [expr]
  (= (class expr) se.raek.Tag))

(defn type [expr]
  (cond (tag? expr)     (key expr)
	(symbol? expr)  ::symbol
	(keyword? expr) ::keyword
	(number? expr)  ::number
	(string? expr)  ::string
	(char? expr)    ::character
	(bool? expr)    ::boolean
	(nil? expr)     ::nil
	(list? expr)    ::list
	(vector? expr)  ::vector
	(map? expr)     ::map
	(set? expr)     ::set
	:else           ::unsupported))

(defn error [message expr]
  (throw (proxy [java.lang.Exception] [(.concat message (pr-str expr))])))

(def *global-env* (atom {}))

(sort
 (keys
  (swap! *global-env* into
	 (concat (map (fn [x] [x (tag ::builtin (clojure.core/eval x))])
		      '(+ - * / = > >= < <= nil? symbol? keyword? list?))
		 (map (fn [x] [x (tag ::special (keyword x))])
		      '(if lambda quote do))
		 [['π Math/PI] ['e Math/E]]
		 (map (fn [[name params code]] [name (tag ::function [params code *global-env*])])
		      '((inc (x) (+ x 1))
			(dec (x) (- x 1))
			(not (x) (if x false true))
			(and (x y) (if x (if y true false) false))
			(or (x y) (if x true (if y true false)))
			(zero? (x) (= x 0))
			(pos? (> x 0))
			(neg? (< x 0))
			(fak (n) (if (= n 0)
				   1
				   (* n (fak (dec n))))))
		      )))))

(derive ::literal ::type)
(derive ::keyword ::literal)
(derive ::number ::literal)
(derive ::string ::literal)
(derive ::character ::literal)
(derive ::boolean ::literal)
(derive ::nil ::literal)
(derive ::literal ::type)
(derive ::vector ::collection)
(derive ::map ::collection)
(derive ::set ::collection)
(derive ::collection ::type)
(derive ::function ::internal)
(derive ::special ::internal)
(derive ::builtin ::internal)
(derive ::internal ::type)

(defmulti eval (fn [expr env] (type expr)))
(defmulti apply (fn [f args] (type f)))
(defmulti special (fn [f args env] (val f)))

(defn top-eval [expr]
  (eval expr *global-env*))

(defmethod eval ::literal [lit env]
  lit)

(defmethod eval ::symbol [sym env]
  (if (contains? @env sym)
    (get @env sym)
    (error "Unbound symbol: " sym)))

(defmethod eval ::list [lst env]
  (let [f (eval (first lst) env)
	args (rest lst)]
    (if (isa? (type f) ::special)
      (special f args env)
      (let [eval-args (vec (map #(eval % env) args))]
	(apply f eval-args)))))

(defmethod eval ::collection [coll env]
  (into (empty coll) (map #(eval % env) coll)))

(defmethod eval :default [expr env]
  (error "Dont know how to evaluate: " expr))

(defmethod apply ::builtin [[_ f] args]
  (clojure.core/apply f args))

(defmethod apply ::function [[_ f] args]
  (let [[params code env] f]
    (eval code (atom (merge @env (zipmap params args))))))

(defmethod apply :default [f args]
  (error "Can't use as function: " f))

(defmethod special :quote [f args env]
  (first args))

(defmethod special :if [f [condition true-branch false-branch] env]
  (if (eval condition env)
    (eval true-branch env)
    (eval false-branch env)))

(defmethod special :lambda [f [params code] env]
  (tag ::function [params code env]))

(defmethod special :do [f args env]
  (last (map #(eval % env) args)))

(defmethod special :default [f args env]
  (error "Unknown special form: " (val f)))
