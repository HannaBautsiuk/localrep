(ns lab1.core
  (:require [clj-http.client :as client])
  (:require [pl.danieljanus.tagsoup :refer :all]))


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
  (let [
          http-response (client/get link {:throw-exceptions false})
          status (getStatusDesc http-response)
          newLinks (if (nil? status)
                     (get-all-links (get http-response :body) link)
                     '())
          ]
    (hash-map :link link :status status :chldLinks newLinks)))

(defn- getResult[links depth]
  (loop [curLinks links
         curDept 1
         rez '()]
    (if (> curDept depth)
      rez
      (let [
          curRez (pmap getResultOfLink curLinks)]
        (recur (mapcat #(get % :chldLinks) curRez)
        (+ curDept 1)
        (into rez curRez))))))

(defn- printResult [res]
  (do
    (print (str (get res :link) " "))
    (if (nil? (get res :status))
      (println (str (count (get res :chldLinks)) " " ))
      (println (str (get res :status) " " )))))

(defn- process[links depth]
    (dorun (map printResult (getResult links depth))))

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
