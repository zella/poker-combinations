name := """poker-combinations"""

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.8"

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "3.0.1"

// https://mvnrepository.com/artifact/com.fasterxml.jackson.core/jackson-databind
libraryDependencies += "com.fasterxml.jackson.core" % "jackson-databind" % "2.8.5"
// https://mvnrepository.com/artifact/com.fasterxml.jackson.datatype/jackson-datatype-jsr310
libraryDependencies += "com.fasterxml.jackson.datatype" % "jackson-datatype-jsr310" % "2.8.5"
// https://mvnrepository.com/artifact/com.fasterxml.jackson.datatype/jackson-datatype-jdk8
libraryDependencies += "com.fasterxml.jackson.datatype" % "jackson-datatype-jdk8" % "2.8.5"