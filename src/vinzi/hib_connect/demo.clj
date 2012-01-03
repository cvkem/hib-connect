(ns vinzi.hib-connect.demo
  (use vinzi.hib-connect.core)
  (import vinzi.cdfdeMgt.domain.TrackInfo))


(defn create-track-record [session val1 val2]
  (let [tr (TrackInfo. val1 val2)]
    (println  "saving the record with contents: " tr)
    (.save session tr)))


(defn read-all [session]
  (let [qryStr "from TrackInfo"
	qry   (.createQuery session qryStr)
	res   (.list qry)]
    (println "The query has type: " (type qry))
    (println "The resultset had type: " (type res))
    (println "The results are")
    (println res)))

(defn demo []
  (println "Adding a first record (of this session)")
  (call-in-hib-session create-track-record "Hi"  "this is the first record" )

  (println " Adding a few more records")
  (call-in-hib-session (fn [s] (doall (map #(create-track-record s "count-up" (str "count="%)) (range 3)))))

  (println "Reading all contents")
  (call-in-hib-session read-all)
 
  (println " closing down the hibernate session (was opened automatically")
  (close-hib)
  )


