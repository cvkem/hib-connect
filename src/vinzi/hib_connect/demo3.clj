(ns vinzi.hib-connect.demo3
  (:use [vinzi.hib-connect bridge code-generator])
  (:use [vinzi.test int_rec str_rec]))


(defn demo3 []
  (force-open-hib)
  (println "\n\nDid you already run demo-1 and demo-2 from demo.clj"
	   "The output of these demo's is needed to start this demo")
  (let [s1 (create-str_rec "hello" "world")
	s2 (create-str_rec "HI"   "THERE")
	i1 (create-int_rec  1    2)
	i2 (create-int_rec  500 1000)]

    (println "store a str_rec: " s1)
    (call-in-hib-session store-hib  s1)
    (println "store a int_rec" i1)
    (let [res (call-in-hib-session store-hib  i1)]
      (println " i1 = " i1)
      (println "  after storing received result = " res)
;; currently not possible due to type-conversion
;;      (call-in-hib-session store-hib  (assoc res :a -1000))
;;      (println "stored i1 again with value -1000")
      )

    (let [res (call-in-hib-session store-hib  i1)]
      (println "stored i2 = " i2)
      (println "  after storing received result = " res)
      (call-in-hib-session store-hib  res)
      (println "stored res of i2 again (overwrites previous value)"))
    
    (println "store a (mixed sequence" i1  s2  s1)
    (call-in-hib-session store-hib  [i1 s2 s1])

    (print "generate a sequence of objects")
    (letfn [(gen-func-map [session range func]
			  (doseq [i range]
			    (let [ir (create-int_rec i (func i))]
			      (store-hib session ir))))]
      (call-in-hib-session gen-func-map (range 30) #(* % %)))

    (println "Delete item a=3 with a HQL query")
    (call-in-hib-session delete-hib "FROM int_rec_jv WHERE a = 3")

    (println " Select item a=5 and subsequently delete it")
    (with-query-results res ["FROM int_rec_jv WHERE a = 5"]
      (call-in-hib-session delete-hib res))
      ;;  (println " closing down the hibernate session (was opened automatically")
  (close-hib)
  ))

(defn demo4 []
  (force-open-hib)
  (println "requires that demo3 has run")
  (with-query-results res ["FROM int_rec_jv WHERE a = 1"]
    (doseq [r res]
      (println r))))
