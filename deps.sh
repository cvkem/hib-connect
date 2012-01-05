echo
echo Resolving depencies
lein deps

echo
echo Make classes directory
mkdir classes

echo
echo compile the java-sources
tools/jc src/java/*.java

echo READY 
