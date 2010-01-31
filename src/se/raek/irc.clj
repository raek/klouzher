(ns se.raek.irc)

(def *message-pattern* #"(?::([^ ]+) )?([^ ]+)((?: [^ :][^ ]*)*)(?: :(.*))?")
(def *params-pattern* #" ([^ ]+)")
(def *peer-pattern* #"([#&].*)|(([^!@]+)(?:!([^!@]+))?(?:@([^!@]+))?)|([^!@]+\\.[^!@]+)")

(defn parse-peer [string]
  (let [[_ channel client nickname username hostname server] (re-find *peer-pattern* string)]
    (cond
      channel {:type :channel, :channel channel}
      client {:type :client, :nickname nickname, :username username, :hostname hostname}
      server {:type :server, :server server})))

(defmulti interpret-message (fn [command source params] command))

(defn reply-to [source dest]
  (if (= (:type dest) :channel)
    dest
    source))

(defmethod interpret-message "PRIVMSG" [command source [dest text]]
  {:dest (parse-peer dest)
   :text text
   :reply-to (reply-to source dest)})

(defmethod interpret-message "NOTICE" [command source [dest text]]
  {:dest (parse-peer dest)
   :text text})

(defn parse-message [string]
  (let [[_ prefix command params-string trailing] (re-find *message-pattern* string)
	params (concat (map second
			    (re-seq *params-pattern* params-string))
		       [trailing])
	source (parse-peer prefix)]
    (merge {:type command
	    :source source
	    :params params}
	   (interpret-message command source params))))

(defn format-message [message]
  :foo)