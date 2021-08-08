mkdir fula
mv Helper.java fula
javac fula/Helper.java
mkdir tmp
cd tmp
unzip -uo ../scala-lib.jar
cd ../
jar cf fular.jar fula -C tmp .
mv fula/Helper.java .
rm -rf fula/
rm -rf tmp/
