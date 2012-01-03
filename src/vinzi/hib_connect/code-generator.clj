(ns vinzi.hib-connect.code-generator
  (require [clojure.string :as str])
  (import  [java.io File]))


(defn write-src-file
  "Write a file with the provided contents."
  [fileName contents]
  (let [file (File. fileName)]
    (print " file '" fileName "'")
    (if (.exists file)
      (println "  exists already and will be overwritten")
      (println "  is a new file"))
    (spit file contents)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions to generate a java source-file that defines the
;; class (objects) that will be stored in hibernate)
;;

(def javaSuffix "_jv")
(def headerFmt "package %s;\n
public class %s {")
(def fieldFmt "
    public %s %s;
    public %s get%s() { return %s;}
    public void set%s(%s %s)  {this.%s = %s; };\n\n")
(def constructFmt "
    public %s() {};\n
    public %s(%s) {\n%s\treturn;};\n\n")
(def toStringFmt "   public String toString() {
        return %s;};\n\n")
(def footFmt "};\n")


(defn getSet-postfix
  "Getters and setters need a fieldname where the first character
   is in upper-case."
  [name]
  (apply str (str/upper-case (first name)) (rest name)))

(defn java-field-def
  "Produce a string with a type-definition, a getter and a setter."
  [[fName fType]]
  (let [gsName (getSet-postfix fName)]
    (format fieldFmt fType fName
	    fType gsName fName
	    gsName fType fName fName fName)))

(defn java-constructor-def
  "Define a default constructor and a constructor that takes all parameters."
  [name fields]
  (let [args (map #(str (second %) " " (first %)) fields)
	args (apply str (interpose ", " args))
	assign (map #(format "\tthis.%s = %s;\n" %1 %1) (map first fields))
	assign (apply str assign)]
    (format constructFmt name name args assign)))

(defn java-toString
  "Define a toString function that prints the fields prefixed by its name"
  [fields]
  (let [fields (map first fields)
	toString  (map #(format "\" %s=\" + this.%s" %1 %1) fields)
	toString (apply str (interpose " + " toString))]
    (format toStringFmt toString)))

(defn gen-java [package name fields]
  (let [head    (format headerFmt package name)
	fldStr  (map java-field-def fields)
	fldStr  (apply str fldStr)
	cstrStr (java-constructor-def name fields)
	toStr   (java-toString fields)
	fileName `("src" "java" ~(str name ".java"))
	fileName (apply str (interpose File/separator fileName))]
    (write-src-file fileName (str head fldStr cstrStr toStr footFmt))))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions to generate a clojure source-file that defines the
;; records (hash-maps) used in clojure and the translators to
;; between clojure-hasmaps and java-objects.



(def cljHeadFmt  "(ns %s.%s
    (use vinzi.hib-connect.globals)
    (import %s.%s))\n\n")
(def defrecordFmt "(defrecord %s [%s])\n\n")
(def cljToJavaFmt "(defn %s-to-java [cRec]\n\t(%s. %s))\n\n
(add-to-java (%s. %s) %s-to-java)\n\n")
;(def cljCreateFmt "(def _type {:to-java %s-to-java})\n
;(defn create-%s [%s]
;  (%s. _type %s))\n\n")
(def javaToCljFmt "(defn %s-to-clj [jRec]\n\t(%s. %s))\n\n
(add-to-clj (%s.) %s-to-clj)\n\n")

(defn clj-head [package name javaName]
     (format cljHeadFmt package name package javaName))

(defn clj-defrecord
  "Define the defrecord for the clj-side of the datastructure."
  [name fNames]
  (let [flds (apply str (interpose " " fNames))]
    (format defrecordFmt name flds)))

(defn clj-to-java
  "Define a translator that maps a clojure defrecord to a java class."
  [name fNames javaName]
  (let [fields (map #(format "(:%s cRec) " %) fNames)
	fields (apply str fields)
	args   (apply str (interpose " " (range (count fNames))))]
    (format cljToJavaFmt name javaName fields
	    name args name)))


;(defn clj-create [name fNames]
;  (let [fNames (apply str (interpose " " fNames))]
;    (format cljCreateFmt name
;	    name fNames
;	    name fNames)))

(defn java-to-clj
  "Define a translator that maps a clojure defrecord to a java class."
  [name fNames javaName]
  (let [gsNames (map getSet-postfix fNames)
	fields (map #(format "(.get%s jRec) " %) gsNames)
	fields (apply str fields)]
    (format javaToCljFmt name name fields
	    javaName name)))

(defn gen-clj
  "Generate a .clj file with the clojure definitions."
  [package name fields javaName]
  (let [head  (clj-head package name javaName)
	fNames (map first fields)
	defrecStr (clj-defrecord name fNames)
	defToJavaStr (clj-to-java name fNames javaName)
;	createStr  (clj-create name fNames)
	defToCljStr  (java-to-clj name fNames javaName)
	fileName (concat (list "src")
			 (str/split package #"\.")
			 (list (str name ".clj")))
	fileName (apply str (interpose File/separator fileName))
	]
    (write-src-file fileName (str head defrecStr
		 defToJavaStr defToCljStr))))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions to generate mapping that is used by hiberate.
;;

(def hibHeadFmt "<hibernate-mapping package=\"%s\">
\t<class name=\"%s\" table=\"%s\">
\t\t<id name=\"id\" column=\"id\">
\t\t\t<generator class=\"native\"/>
\t\t</id>\n")
(def hibFootFmt "\t</class>\n</hibernate-mapping>")

(defn gen-hib [package name fields javaName]
  (let [head (format hibHeadFmt package javaName (str/lower-case name))
	;; skip the 'id' fields (is already pre-defined)
	fields (if (= (ffirst fields) "id") (rest fields) fields)
	fldStr (map #(format "\t\t<property name=\"%s\"/>\n" %)
		    (map first fields))
	fldStr (apply str fldStr)
	fileName `("resources"  ~(str name ".xml.include"))
	fileName (apply str (interpose File/separator fileName))]
    (write-src-file fileName (str head fldStr hibFootFmt))))




(defn generate-hibernate-code
  "Generate the code for a hibernate connector. This consists of:
    1. A java file defining the java-object that will be stored.
    2. A clojure file with the corresponding defrecord and helper functions.
    3. A hibernate mapping (inclusion) file."
  [qualifiedName fields]
  (let [nameParts (str/split qualifiedName #"\.")
	name  (last nameParts)
	package (take (dec (count nameParts)) nameParts)
	package (apply str (interpose "." package))
	javaName (str name javaSuffix)
	;; prepend an id-fields
	fields  (concat (list ["id" "long"]) fields)]
    (gen-java package javaName fields)
    (gen-clj package name fields javaName)
    (gen-hib package name fields javaName)
    ))

