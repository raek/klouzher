(ns se.raek.bot
  (:use [se.raek.irc]
	[se.raek.irc-connection]
	[se.raek.events])
  (:require [se.raek.pyttelisp :as pl]
	    [se.raek.html :as html]))

(def *self* {:type :client
	     :nickname "klouzher"
	     :username "klouzher"
	     :hostname "*"
	     :realname "Raek's clojure bot"})

(def *connection-configurations*
     {:local {:type :server
	      :host "localhost"
	      :port 9001
	      :self *self*}})

(def *event-dispatcher* (agent {}))

(defn do-thread [f & args]
  (let [thread (new Thread (fn [] (apply f args)))]
    (.start thread)))

(defn message-event-name [message]
  (keyword (.concat "on-" (.toLowerCase (:command message)))))

(defn handle-message [message]
  (send *event-dispatcher* dispatch-event :on-message message)
  (send *event-dispatcher* dispatch-event (message-event-name message) message))

(defn wait-for-motd [messages]
  (when-first [message messages]
    (condp = (:command message)
      "PING" (do (send-message (connection message) (pong (:token message)))
		 (recur (rest messages)))
      "001" (do (send *event-dispatcher* dispatch-event :on-connect (connection message))
		messages)
      (recur (rest messages)))))

(defn handle-connection [conn]
  (let [config (get *connection-configurations* (label conn))
	self (:self config)]
    (send-message conn (nick (:nickname self)))
    (send-message conn (user (:username self) "*" "*" (:realname self)))
    (dorun (map handle-message (wait-for-motd (receive-seq conn))))))

(defn auto-pong [{:as message, token :token}]
  (send-message (connection message) (pong (:token message))))

(defn auto-join [conn]
  (send-message conn (join "#d1d")))

(defn highlighted [text]
  "hey!")

(defn pyttelisp [expr]
  (pr-str (pl/top-eval expr)))

(defn title [text]
  (pyttelisp (list 'title text)))

(defn react [{:as message, text :text}]
  (let [result (cond (.startsWith text "=") (pyttelisp (read-string (.substring text 1)))
		     (.startsWith text (:nickname *self*)) (highlighted text))
		     ;(re-find #"^https?://[^ ]+$" text) (do (println text) (title text)))
	line (if (> (.length result) 400)
	       (.concat (.substring result 0 400) "... *snip*")
	       result)]
    (when line
      (send-message (connection message) (privmsg (:reply-to message) line)))))

(defn bozo [{:as message, line :line}]
  (println (.concat "---BOZO " line)))

(do (dorun (map (fn [[event-type key handler]] (send *event-dispatcher* add-event-handler event-type key handler))
		[[:on-connect :auto-join #'auto-join]
		 [:on-ping :auto-pong #'auto-pong]
		 [:on-privmsg :react #'react]]))
    :ok)

(dosync (pl/add-builtins pl/*global-env* '(html/parse html/title))
	(pl/add-function-definitions pl/*global-env* '((title (url) (html/title (html/parse url))))))

;(def l (make-connection "localhost" 9001 :local))
;(do-thread handle-connection l)
;(def q (make-connection "irc.quakenet.org" 6669 :local))
;(do-thread handle-connection q)