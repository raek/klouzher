(ns se.raek.coding
  (:import [java.nio ByteBuffer CharBuffer]
	   [java.nio.charset Charset CodingErrorAction
	    MalformedInputException]))

(defstruct charset-struct :name :encoder :decoder)

(defn make-charset [name]
  (let [charset (Charset/forName name)]
    (struct-map charset-struct
      :name (.name charset)
      :encoder (.. charset
		   (newEncoder)
		   (onUnmappableCharacter CodingErrorAction/REPLACE))
      :decoder (.. charset
		   (newDecoder)
		   (onMalformedInput CodingErrorAction/REPORT)))))

(defn encode [string charset]
  (let [char-buffer (CharBuffer/wrap string)
	byte-buffer (.. (:encoder charset) (encode char-buffer))
	bytes (byte-array (.remaining byte-buffer))]
    (.get byte-buffer bytes 0 (.remaining byte-buffer))
    bytes))

(defn decode [bytes charset]
  (try (let [byte-buffer (ByteBuffer/wrap bytes)]
	 (.. (:decoder charset) (decode byte-buffer) toString))
       (catch MalformedInputException _
	 nil)))

(defn try-decode [bytes charsets]
  (when (seq charsets)
    (if-let [string (decode bytes (first charsets))]
      [string (first charsets)]
      (recur bytes (rest charsets)))))
