(ns vinzi.test.TestClass
    (use vinzi.hib-connect.globals)
    (import vinzi.test.TestClass_jv))

(defrecord TestClass [id val1 val2])

(defn TestClass-to-java [cRec]
	(TestClass_jv. (:id cRec) (:val1 cRec) (:val2 cRec) ))


(add-to-java (TestClass. 0 1 2) TestClass-to-java)

(defn TestClass-to-clj [jRec]
	(TestClass. (.getId jRec) (.getVal1 jRec) (.getVal2 jRec) ))


(add-to-clj (TestClass_jv.) TestClass-to-clj)

