(ns se.raek.agnostic-connection
  (:import [java.io InputStreamReader BufferedReader BufferedOutputStream
	    IOException]
	   [java.net Socket]))

(defn make-connection [host port]
  (let [socket (Socket. host port)
	in (-> socket .getInputStream (InputStreamReader. "ISO-8859-1")
	       (BufferedReader.))
	out (-> socket .getOutputStream BufferedOutputStream.)
	receive-line (fn []
		       (locking in
			 (try
			  (when-let [line (.readLine in)]
			    (.getBytes line "ISO-8859-1"))
			  (catch IOException _ nil))))
	send-line (fn [bytes]
		    (locking out
		      (doto out
			(.write bytes 0 (alength bytes))
			(.write 0x0D)
			(.write 0x0A)
			(.flush))))
	close (fn []
		(.close socket))]
    {:receive-line receive-line
     :send-line send-line
     :close close}))
