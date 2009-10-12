(ns com.github.raek.events
    (:use [clojure.contrib.core :only [dissoc-in]]))

(defn add-event-handler [state event-type key-or-agent f]
  (assoc-in state [::handlers event-type key-or-agent] f))

(defn remove-event-handler [state event-type key-or-agent]
  (dissoc-in state [::handlers event-type key-or-agent]))

(defn dispatch-event [state event-type & args]
  (doseq [[key-or-agent f] (-> state ::handlers event-type)]
      (if (keyword? key-or-agent)
	(apply f args)
	(send-off key-or-agent f args)))
  state)
