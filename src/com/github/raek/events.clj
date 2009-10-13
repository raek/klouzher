;; Copyright (c) Rasmus Svensson. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

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
	(send key-or-agent f args)))
  state)
