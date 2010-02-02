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
    by an agent. Event dispatchers store their handlers in a map, which may
    be place in one of the reference types.
    
    The functions ins this library are used to change the state of an event
    dispatcher. They can be called directly with the map (containing its event
    handlers) as its first argument directly, or be used to mutate the state
    of a reference type with swap!, alter, commute, send or send-off.
    
    When an event is dispatched, and a handler is invoked its function is
    either sent to an agent or run by the event dispatcher itself. A event
    handler that is run by the dispatcher should be short and non-blocking. If
    a time consuming or blocking operation is necessary, it should be executed
    in a separate thread (e.g. sent to an agent with send-off), since the
    other handlers may want to be notified about the event as soon as
    possible.
    
    The event handler function will receive the event type as its first
    argument followed by any arguments passed to dispatch-event."}
    se.raek.events
    (:use [clojure.contrib.core :only [dissoc-in]]))

(defn add-event-handler
  "Adds an event handler to an event dispatcher.
  
  If key-or-agent is an agent, the handler function f will be sent to that
  agent when the event occurs. Otherwise, f will be run by the thread that
  dispatches the event."
  [handlers event-type key-or-agent f]
  (assoc-in handlers [event-type key-or-agent] f))

(defn remove-event-handler [handlers event-type key-or-agent]
  "Remove an event handler from an event dispatcher."
  (dissoc-in handlers [event-type key-or-agent]))

(defn dispatch-event [handlers event-type & args]
  "Dispatches an event."
  (doseq [[key-or-agent f] (get handlers event-type)]
      (if (keyword? key-or-agent)
	(apply f (cons event-type args))
	(apply send key-or-agent f (cons event-type args))))
  handlers)
