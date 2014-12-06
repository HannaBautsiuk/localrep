(ns lab1.core
  (:require [clj-http.client :as client])
  (:require [pl.danieljanus.tagsoup :refer :all]))

(def results (atom '()))
(def usedLinks (atom '()))

(defn- getAbsLink[link baseLink]
  (cond 
    (clojure.string/blank? link) nil
    (.startsWith (.toLowerCase link) "http") link
    :else (str baseLink (subs link 1))))

(defn- link [vect]
  (if (and
        (vector? vect)
        (= :a (first vect)))
    (remove nil? [(get-in vect [1 :href])])
    []))

(defn get-all-links[htmlString baseLink]
  (->> (parse-string htmlString)
       (tree-seq vector? children)
       (filter vector?)
       (mapcat link)
       (pmap #(getAbsLink % baseLink))))

(defn getStatusDesc[http-response]
  (let [status (get http-response :status)]
  (cond
    (= 200 status) nil
    (or (= 301 status) (= 300 status)) "redir"
    (= 404 status) "bad"
    :else "unknown error")))

(defn- getResultOfLink[link]
  (let [http-response (client/get link {:throw-exceptions false})
        status (getStatusDesc http-response)
        newLinks (if (nil? status)
                     (get-all-links (get http-response :body) link)
                      '())
        rez (hash-map :link link :status status :chldLinks newLinks)
        ]
    (swap! results conj rez)
    (swap! usedLinks conj link)
    rez
    ))

(defn is-new-link? [target] 
  (not (some #(= target %) @usedLinks)))

(defn- getNewLinks[node]
  (->> (:chldLinks node)
       (filter is-new-link?)))

(defn- getResult[curLinks depth]
  (let [new-depth (dec depth)]
    (if (< depth 1)
      nil
     (doseq [curRez (pmap getResultOfLink curLinks)] 
       (getResult (distinct (filter is-new-link? (:chldLinks curRez))) new-depth)))))

(defn- printResult [res]
  (do
    (print (str (:link res) " "))
    (if (nil? (:status res))
      (println (str (count (:chldLinks res)) " " ))
      (println (str (:status res ) " " )))))

(defn- printAllResult []
  (dorun (map printResult @results)))

(defn- process[links depth]
    (getResult links depth)
    (printAllResult))

(defn- input-params-incorrect[file-name depth]
  (or
    (nil? file-name)
    (not (.exists (clojure.java.io/as-file file-name)))
    (nil? depth)
    (not (number? depth))
    (< depth 1)))

(defn -main [file-name depth]
   (if (input-params-incorrect file-name depth)
    (println "Incorrect start params")
    (do
      (with-open [rdr (clojure.java.io/reader file-name)]
            (process (line-seq rdr) depth)))))
