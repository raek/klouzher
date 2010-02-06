(ns se.raek.irc)

;; message -> (prefix command params trailing)
(def *message-pattern* #"(?::([^ ]+) )?([^ ]+)((?: [^ :][^ ]*)*)(?: :(.*))?")
(def *params-pattern* #"[^ ]+")
;; peer -> (channel client nickname username hostname server)
(def *peer-pattern* #"([#&].*)|(([^!@]+)(?:!([^!@]+))?(?:@([^!@]+))?)|([^!@]+\\.[^!@]+)")

;; Messages
;;
;; Messages normally have at least three keys: :command, :source and
;; :params. :command, :source and the elements of :params (which is a vector)
;; are strings in the form that they appear in the underlying IRC protocol.
;;
;; When a message is made, values for additional keys are often set. The
;; meaning of these depend on which command the message signifies. They don't
;; have to be strings and do often hold data structures constructed from the
;; params.
(defstruct message-struct :command :source :params)

;; Peers
;;
;; Peers are the generalization of channels, clients (persons/bots), and
;; servers. The one thing they have in common is their ability to send and
;; receive messages on IRC.
(defstruct channel-struct :type :channel)
(defstruct client-struct :type :nickname :username :hostname)
(defstruct server-struct :type :server)

(defn account [hostname]
  (when hostname
    (when-let [[_ user] (re-find #"(.*)\.users\.quakenet\.org" hostname)]
      (keyword user))))

(defn parse-peer [string]
  (let [[_ channel client nickname username hostname server] (re-find *peer-pattern* string)]
    (cond
      channel (struct channel-struct :channel channel)
      client (assoc (struct client-struct :client nickname username hostname)
	       :account (account hostname))
      server (struct server-struct :server server))))

(defn format-peer [peer]
  (condp = (:type peer)
    :channel (:channel peer)
    :client (:nickname peer)
    :server (:server peer)))

(defmulti interpret-message (fn [command source params] command))

(defn reply-to [source dest]
  (if (= (:type dest) :channel)
    dest
    source))

(defmethod interpret-message "PRIVMSG" [command source [dest text]]
  (let [dest (parse-peer dest)]
    {:dest dest
     :text text
     :reply-to (reply-to source dest)}))

(defn privmsg [dest text]
  (let [dest (if (string? dest)
	       (parse-peer dest)
	       dest)]
    (merge (struct message-struct "PRIVMSG" nil [(format-peer dest) text])
	   {:dest dest, :text text})))

(defmethod interpret-message "NOTICE" [command source [dest text]]
  {:dest (parse-peer dest)
   :text text})

(defn notice [dest text]
  (let [dest (if (string? dest)
	       (parse-peer dest)
	       dest)]
    (merge (struct message-struct "NOTICE" nil [(format-peer dest) text])
	   {:dest dest, :text text})))

(defmethod interpret-message "PING" [command source [token]]
  {:token token})

(defn ping [token]
  (merge (struct message-struct "PING" nil [token])
	 {:token token}))

(defmethod interpret-message "PONG" [command source [token]]
  {:token token})

(defn pong [token]
  (merge (struct message-struct "PONG" nil [token])
	 {:token token}))

(defn nick [nickname]
  (merge (struct message-struct "NICK" nil [nickname])
	 {:nickname nickname}))

(defn user [username hostname servername realname]
  (merge (struct message-struct "USER" nil [username hostname servername realname])
	 {:username username, :hostname hostname,
	  :servername servername :realname realname}))

(defn join [channel]
  (let [channel (if (string? channel)
	       (parse-peer channel)
	       channel)]
    (merge (struct message-struct "JOIN" nil [(format-peer channel)])
	   {:channel channel})))

(defmethod interpret-message :default [command source [dest text]] {})

(defn parse-message
  "Parses an IRC message given as a string into a IRC message structure."
  [line]
  (if-let [[_ prefix command params-string trailing] (re-find *message-pattern* line)]
    (let  [first-params (re-seq *params-pattern* params-string)
	   params (if trailing
		    (concat first-params [trailing])
		    first-params)
	   source (when prefix (parse-peer prefix))]
      (merge (struct message-struct command source params)
	     (interpret-message command source params)))
    (merge (struct message-struct "BOZO" nil [line])
	   {:line line})))

(defn sanitize-param [param]
  (-> (str param)
      (.replace \return \_)
      (.replace \newline \_)
      (.replace \space \_)))

(defn sanitize-trailing-param [param]
  (-> (str param)
      (.replace \return \space)
      (.replace \newline \space)))

(defn format-message [message]
  (let [command (sanitize-param (:command message))
	params (apply str (interpose \space
				     (map sanitize-param
					  (butlast (:params message)))))
	trailing (sanitize-trailing-param (last (:params message)))]
    (cond (= trailing "") command
	  (= params "") (format "%s :%s" command trailing)
	  :esle (format "%s %s :%s" command params trailing))))
