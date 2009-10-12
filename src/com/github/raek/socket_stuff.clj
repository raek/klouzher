;; This is not by any means tidy. Don't look at it for too long.

(ns socket-stuff
    (:import (java.net Socket InetSocketAddress SocketTimeoutException)
	     (java.io InputStreamReader OutputStreamWriter
		      BufferedReader BufferedWriter
		      IOException)
	     (java.nio ByteBuffer)
	     (java.nio.charset Charset CodingErrorAction
			       MalformedInputException)))

(defstruct charset-struct :name :encoder :decoder)

(defn make-charset-struct [name]
  (let [charset (Charset/forName name)]
    (struct-map charset-struct
		:name (keyword (.name charset))
		:encoder (.newEncoder charset)
		:decoder (.. charset
			     (newDecoder)
			     (onMalformedInput CodingErrorAction/REPORT)))))

(def *default-charset* (make-charset-struct "UTF-8"))
(def *fallback-charset* (make-charset-struct "ISO-8859-1"))

(defn smart-decode [bytes]
  (try
   (let [byte-buffer (ByteBuffer/wrap bytes)]
     [(.toString (.decode (*default-charset* :decoder) byte-buffer))
      (*default-charset* :name)])
   (catch MalformedInputException _
	  (let [byte-buffer (ByteBuffer/wrap bytes)]
	    [(.toString (.decode (*fallback-charset* :decoder) byte-buffer))
	     (*fallback-charset* :name)]))))

(def *connect-timeout* 10000)
(def *read-timeout* 3000)

(defstruct connection-struct :socket :reader :writers :loop-read)

(defn setup-reader [{socket :socket :as state}]
  (assoc state :reader (-> socket (.getInputStream)
			   (InputStreamReader. "ISO-8859-1")
			   (BufferedReader.))))

(defn setup-writers [{socket :socket :as state}]
  (let
      [default-writer (-> socket (.getOutputStream)
			  (OutputStreamWriter. (*default-charset* :encoder))
			  (BufferedWriter.))
       fallback-writer (-> socket (.getOutputStream)
			   (OutputStreamWriter. (*fallback-charset* :encoder))
			   (BufferedWriter.))]
    (assoc state :writers
	   {(*default-charset* :name) default-writer,
	   (*fallback-charset* :name) fallback-writer})))

(defn connect [_ host port]
  (let [socket (doto (Socket.)
		 (.connect (InetSocketAddress. host port) *connect-timeout*)
		 (.setSoTimeout *read-timeout*))]
    (-> (struct connection-struct) (assoc :socket socket :read-loop true)
	(setup-reader) (setup-writers))))

(defn disconnect [{socket :socket}]
  (.close socket)
  (struct connection-struct))

(defn read-line [{:keys [reader read-loop] :as state} f]
  (try
   (let [bytes (.. reader redLine (getBytes "ISO-8859-1"))
	 [line charset] (smart-decode bytes)]
     (f {:connection *agent*, :charset charset} line))
   (catch SocketTimeoutException _
	  (f {:connection *agent*, :charset :UTF-8} "<<timeout>>"))
   (catch IOException _))
  (when read-loop (send-off *agent* read-line f))
  state)

(defn on-read-line [{:keys [connection charset] :as context} line]
  (send-off connection write-line (str line "\n") charset))

(defn auto-reply [{:keys [running reader writers] :as state}]
  (try (let [[s charset] (smart-decode (.. reader readLine
					   (getBytes "ISO-8859-1")))]
	 (doto writer (.write s) .newLine .flush))
       (catch SocketTimeoutException _
	      (doto writer (.write "*") .flush)))
  (if running
    (send-off *agent* auto-reply)
    (println "auto-reply quitting"))
  state)

(defn stop [state]
  (assoc state :running false))

(defn start [state]
  (assoc state :running true))

(def *running* (ref false))

(defn set-running! [state]
  (dosync (ref-set *running* state)))