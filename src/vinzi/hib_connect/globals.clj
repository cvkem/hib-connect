(ns vinzi.hib-connect.globals)

(def to-java (atom {}))
(def to-clj (atom {}))

(defn get-type-id
  "The 'get-type-id' is used to retrieve the class/type of a function.
   It takes the 'str' of type because otherwise multiple definitions
   result in different keys (however, the string-representation is the same)."
  [obj]
  ;; Warning: don't use keywords, as the keyword of type always equals nil
  (str (type obj)))

(defn add-to-clj
  "The type of 'obj' is determined and it is stored in the atom 'to-clj'."
  [obj translateFunc]
  (let [cls (get-type-id obj)] 
    (swap! to-clj #(assoc % cls translateFunc))))

(defn get-to-clj
  "Look up the translator to tranforma java-object 'obj' to a clojure structure"
  [obj]
  (let [cls (get-type-id obj)
	func (get @to-clj cls)]
    (when (nil? func)
      (println "The translator to clojure for class " cls " could not be found!")
      (assert func))
    func))

(defn add-to-java
  "The type of 'obj' is determined and it is stored in the atom 'to-java'."
  [obj translateFunc]
  (let [cls (get-type-id obj)] 
    (swap! to-java #(assoc % cls translateFunc))))

(defn get-to-java
  "Look up the translator to tranforma java-object 'obj' to a clojure structure"
  [obj]
  (let [cls (get-type-id obj)
	func (get @to-java cls)]
    (when (nil? func)
      (println "The translator to clojure for '" cls "' could not be found!")
      (println "  the object is: " obj)
      (assert func))
    func))