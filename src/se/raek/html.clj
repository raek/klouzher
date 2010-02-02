(ns se.raek.html
  (:import [org.ccil.cowan.tagsoup.jaxp SAXFactoryImpl])
  (:require [clojure.xml :as xml]))

(defn startparse-tagsoup [s ch]
  (.. (new SAXFactoryImpl) (newSAXParser) (parse s ch)))

(defn parse [s]
  (xml/parse s startparse-tagsoup))

(defn title [h]
  (let [head-content (-> h :content first :content)]
    (-> (filter (fn [node] (= (:tag node) :title))
		head-content)
	first :content first .trim)))
