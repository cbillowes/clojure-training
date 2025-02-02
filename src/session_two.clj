(ns session-two
  (:require [clojure.java.io :as io]))


(defn read-lines-from-file!
  "Returns a vector of strings"
  ([]
   (read-lines-from-file! "Orders.csv"))
  ([filename]
   (with-open [rdr (io/reader filename)]
     (into [] (doall (line-seq rdr))))))


; To get all the lines
(read-lines-from-file!)


; To get n number of lines
(take 2 (read-lines-from-file!))