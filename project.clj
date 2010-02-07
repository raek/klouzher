(defproject klouzher "0.1.0-SNAPSHOT"
  :description "an IRC bot with an interpreter for a Lisp-like language written in clojure"
  :url "http://github.com/raek/klouzher"
  :dependencies [[org.clojure/clojure "1.1.0-alpha-SNAPSHOT"]
		 [org.clojure/clojure-contrib "1.0-SNAPSHOT"]
		 [org.clojars.raek/tagsoup "1.2"]]
  :dev-dependencies [[leiningen/lein-swank "1.1.0"]]
  :namespaces [se.raek.agnostic-connection
	       se.raek.bot
	       se.raek.coding
	       se.raek.events
	       se.raek.file-ref
	       se.raek.html
	       se.raek.irc
	       se.raek.irc-connection
	       se.raek.pyttelisp])
