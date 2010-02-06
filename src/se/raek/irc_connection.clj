(ns se.raek.irc-connection
  (:require [se.raek.coding :as coding]
	    [se.raek.agnostic-connection :as conn]
	    [se.raek.irc :as irc]))

(def *default-charset* (coding/make-charset "UTF-8"))
(def *fallback-charset* (coding/make-charset "ISO-8859-1"))
(def *charsets* [*default-charset* *fallback-charset*])

(defn make-connection [host port label]
  (let [counter (atom 0)]
    (with-meta (conn/make-connection host port)
      {:host host
       :port port
       :label label
       :seqno (fn [] (swap! counter inc))})))

(defn send-message [conn message]
  (when message
    (let [line (irc/format-message message)
	  charset (get (meta message) :charset *default-charset*)
	  bytes (coding/encode line charset)]
      (println line)
      ((:send-line conn) bytes))))

(defn receive-message [conn]
  (when-let [bytes ((:receive-line conn))]
    (let [[line charset] (coding/try-decode bytes *charsets*)
	  metadata (meta conn)]
      (println line)
      (with-meta (irc/parse-message line)
	{:charset charset
	 :connection conn
	 :seqno ((:seqno metadata))}))))

(defn receive-seq [conn]
  (lazy-seq
    (when-let [message (receive-message conn)]
      (cons message (receive-seq conn)))))

(defn send-seq [conn messages]
  (when (seq messages)
    (send-message conn (first messages))
    (send-seq conn (rest messages))))

(defn label [conn]
  (:label (meta conn)))

(defn connection [message]
  (:connection (meta message)))