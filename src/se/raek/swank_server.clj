(ns se.raek.swank-server
  (:use [swank.swank])
  (:require [se.raek.bot :as bot]))

(swank.swank/ignore-protocol-version "2009-03-09")
(start-server ".slime-socket" :port 4005 :encoding "utf-8" :dont-close true)
