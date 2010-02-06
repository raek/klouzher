(ns se.raek.pyttelisp
 (:refer-clojure :exclude [type eval apply]))

(defn bool? [x]
  (or (true? x)
      (false? x)))

(defn tag [key val]
  (se.raek.Tag. key val))

(defn tag? [expr]
  (= (class expr) se.raek.Tag))

(def pred-type-map
     {symbol? ::symbol, keyword? ::keyword, number? ::number,
      string? ::string, char? ::character, bool? ::boolean, nil? ::nil,
      list? ::list, vector? ::vector, map? ::map, set? ::set,
      #(= (class %) clojure.lang.Cons) ::list})

(defn pred-get
  "Steps through all the keys in pred-map, which are mutually exclusive
  predicates, finds the first one that returns true when passed expr, and
  returs the value associated with that predicate.
  
  Only one of the predicates should yield true for the given expr. Otherwise,
  this function gives no guarantees regarding which one will be tried first.
  
  (map #(pred-get {neg? -1, zero? 0, pos? 1} %) [5 -2 0]) => (1 -1 0)"
  [pred-map expr]
  (loop [preds (keys pred-map)]
    (when-first [pred preds]
      (if (pred expr)
	(get pred-map pred)
	(recur (rest preds))))))

(defn type [expr]
  (if (tag? expr)
    (key expr)
    (let [type (loop [preds (keys pred-type-map)]
		 (cond (empty? preds) nil
		       ((first preds) expr) (get pred-type-map (first preds))
		       :else (recur (rest preds))))]
      (if type
	type
	::external))))
      
(defn error [message expr]
  (throw (proxy [java.lang.Exception] [(.concat message (pr-str expr))])))

(defn make-env [parent]
  (ref [parent {}]))

(defn find-env
  "Finds the nearest environment in which the symbol is bound."
  [sym env]
  (let [[parent bindings] (deref env)]
    (cond (contains? bindings sym) env
	  (not (nil? parent)) (recur sym parent)
	  :else (error "Unbound symbol: " sym))))

(defn lookup
  "Gives the value of the symbol as of the nearest binding found."
  [sym env]
  (let [[parent bindings] (deref env)]
    (cond (contains? bindings sym) (get bindings sym)
	  (not (nil? parent)) (recur sym parent)
	  :else (error "Unbound symbol: " sym))))

(defn add-binding [env sym value]
  (let [[parent bindings] (deref env)]
    (ref-set env [parent (assoc bindings sym value)]))
  value)

(defn add-bindings [env new-bindings]
  (let [[parent bindings] (deref env)]
    (ref-set env [parent (merge bindings new-bindings)])))

(defn add-builtins [env builtins]
  (add-bindings env (into {} (map (fn [x]
				    [x (tag ::builtin (clojure.core/eval x))])
				  builtins))))

(defn add-special-forms [env specials]
  (add-bindings env (into {} (map (fn [x]
				    [x (tag ::special (keyword x))])
				  specials))))

(defn add-function-definitions [env functions]
  (add-bindings env (into {} (map (fn [[name params code]]
				    [name (tag ::function [params code env])])
				  functions))))

(defn bound-symbols [env]
  (sort (keys (second (deref env)))))

(def *global-env* (make-env nil))

(dosync
 (add-builtins *global-env*
	       '(+ - * / = > >= < <= nil? symbol? keyword? list? print println type get assoc dissoc nth))
 (add-special-forms *global-env*
		    '(if lambda quote do set! def!))
 (add-bindings *global-env*
	       {'π Math/PI, 'e Math/E, 'symbols (tag ::builtin #(bound-symbols *global-env*))})
 (add-function-definitions *global-env*
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
					(* n (fak (dec n)))))))
 (bound-symbols *global-env*))

(doseq [sub [::literal ::collection ::internal]]
  (derive sub ::type))
(doseq [sub [::keyword ::number ::string ::character ::boolean ::nil]]
  (derive sub ::literal))
(doseq [sub [::vector ::map ::set ::list]]
  (derive sub ::collection))
(doseq [sub [::builtin ::special ::function]]
  (derive sub ::internal))

(defmulti eval (fn [expr env] (type expr)))
(defmulti apply (fn [f args] (type f)))
(defmulti special (fn [f args env] (val f)))

(defn top-eval [expr]
  (try
   (dosync (eval expr *global-env*))
   (catch Throwable e (format "Exception Caught: %s: %s" (class e) (.getMessage e)))))

(defmethod eval ::literal [lit env]
  lit)

(defmethod eval ::symbol [sym env]
  (lookup sym env))

(defmethod eval ::list [lst env]
  (let [f (eval (first lst) env)
	args (rest lst)]
    (if (isa? (type f) ::special)
      (special f args env)
      (let [eval-args (vec (map #(eval % env) args))]
	(apply f eval-args)))))

(defmethod eval ::map [coll env]
  (into {} (map (fn [[k v]] [(eval k env) (eval v env)]) coll)))

(defmethod eval ::collection [coll env]
  (into (empty coll) (map #(eval % env) coll)))

(defmethod eval :default [expr env]
  (error "Dont know how to evaluate: " expr))

(defmethod apply ::builtin [[_ f] args]
  (clojure.core/apply f args))

(defmethod apply ::function [[_ f] args]
  (let [[params code env] f]
    (eval code (ref [env (zipmap params args)]))))

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

(defmethod special :def! [f [sym value] env]
  (add-binding env sym (eval value env)))

(defmethod special :set! [f [sym value] env]
  (add-binding (find-env sym env) sym (eval value env)))

(defmethod special :default [f args env]
  (error "Unknown special form: " (val f)))
