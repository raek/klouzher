(ns se.raek.agnostic-connection
  (:import [java.io InputStreamReader BufferedReader BufferedOutputStream]
	   [java.net Socket]))

(defn make-connection [host port]
  (let [socket (Socket. host port)
	in (-> socket .getInputStream (InputStreamReader. "ISO-8859-1")
	       (BufferedReader.))
	out (-> socket .getOutputStream BufferedOutputStream.)]
    (letfn [(receive-line []
			  (.. in readLine (getBytes "ISO-8859-1")))
	    (send-line [bytes]
		       (doto out
			 (.write bytes 0 (alength bytes))
			 (.write 0x0D)
			 (.write 0x0A)
			 (.flush))
		       nil)
	    (close []
		   (.close socket))]
      {:receive-line receive-line
       :send-line send-line
       :close close})))