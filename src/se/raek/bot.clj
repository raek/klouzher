(ns se.raek.bot
  (:require [se.raek.agnostic-connection :as conn]
	    [se.raek.coding :as coding]
	    [se.raek.irc :as irc]))

(def *quakenet-self* {:type :client
		      :nickname "klouzher"
		      :username "klouzher"
		      :hostname "raek.se"
		      :realname "Raek's Clojure Bot"})

(def *unilang-self* {:type :client
		     :nickname "püknik"
		     :username "püknik"
		     :hostname "raek.se"
		     :realname "Raek's Volapük Bot"})

(def *config*
     {:connections [{:type :server
		     :label :quakenet
		     :auto false
		     :host "irc.quakenet.org"
		     :port 6669
		     :self *quakenet-self*}
		    {:type :server
		     :label :unilang
		     :auto false
		     :host "irc.unilang.org"
		     :self *unilang-self*}
		    {:type :console
		     :label :console
		     :auto false
		     :self *quakenet-self*}
		    {:type :server
		     :label :localhost
		     :auto true
		     :host "localhost"
		     :port 9001
		     :self *quakenet-self*}]
      :knowledge {"merola" "är jättelång",
		  "gnarph" "äter gaffel",
		  "raek" "kodar rakt in i datan"}})
