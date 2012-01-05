(ns vinzi.hib-connect.demo
  (:use [vinzi.hib-connect bridge code-generator])
  (:import vinzi.cdfdeMgt.domain.TrackInfo))


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

(defn demo1 []
  (println "\n\nYou need to compile the java-source file in"
	   "'src/java/TrackInfo.java' with command:\n"
	   "\t  javac -d classes src/java/TrackInfo.java\n"
	   "After compiling restart the reply to load this file\n")
  (println "Adding a first record (of this session)")
  (call-in-hib-session create-track-record "Hi"  "this is the first record" )

  (println " Adding a few more records")
  (call-in-hib-session (fn [s] (doall (map #(create-track-record s "count-up" (str "count="%)) (range 3)))))

  (println "Reading all contents")
  (call-in-hib-session read-all)
 
  (println " closing down the hibernate session (was opened automatically")
  (close-hib)
  )




(defn demo2[]
  (generate-hibernate-code :vinzi.test.int_rec
			   [[:a   "int"]
			    ["b" :long]])
  (generate-hibernate-code :vinzi.test.str_rec
			   [[:s   :String]
			    [:t :String]])
  (println "\n\nYou need to compile the java-source file in"
	   "'src/java/TrackInfo.java' with command:\n"
	   "\t  javac -d classes src/java/*.java\n"
	   "You also need to include the resources/*.xml.include file"
	   "In the file resources/mapping/hibernateMapping.xml\n"
	   " (the two files having the same package-prefix can be bundled)\n"
	   "After compiling restart the reply to load this file\n")
  (println " The third demo is in file demo3 as this demo requires"
	   " a different namespace")
  )


