;; Copyright (c) Rasmus Svensson, October 2009. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns #^{:author "Rasmus Svensson",
       :doc "Event library
    
    This library is built around two concept: event dispatchers and event
    handlers. Event handlers are functions that are either run directly or run
    by an agent. Event dispatchers store their handlers in a referece, most
    often an agent.
    
    To create a event dispatcher, create an agent (or any other reference type)
    with a map as its state. To add an event handler to it, send (or use
    corresponding reference altering construct) add-event-handler to the
    agent. Events are dispatched by sending dispatch-event to the event
    dispatcher agent.
    
    The handler function is either sent to an agent or run by the event
    dispatcher itself. Event handlers should be short and non-blocking, and if
    a more time consuming or blocking operation is necessary it should be done
    in another thread (e.g. sent to an agent with send-off).
    
    Since event dispatcher most likely contain other state than just it's event
    handlers, this library assumes that the state of the event dispatcher is a
    map and stores the handlers under the key :com.github.raek.events/handlers."}
    com.github.raek.events
    (:use [clojure.contrib.core :only [dissoc-in]]))

(defn add-event-handler
  "Adds an event handler to an event dispatcher.
  
  If key-or-agent is an agent, the handler function f will be sent to that
  agent when the event occurs. Otherwise, f will be run by the thread that
  dispatches the event."
  [state event-type key-or-agent f]
  (assoc-in state [::handlers event-type key-or-agent] f))

(defn remove-event-handler [state event-type key-or-agent]
  "Remove an event handler from an event dispatcher."
  (dissoc-in state [::handlers event-type key-or-agent]))

(defn dispatch-event [state event-type & args]
  "Dispatches an event."
  (doseq [[key-or-agent f] (-> state ::handlers event-type)]
      (if (keyword? key-or-agent)
	(apply f args)
	(apply send key-or-agent f args)))
  state)
