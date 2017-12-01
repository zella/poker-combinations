name := """poker-combinations"""

version := "0.1"

scalaVersion := "2.12.4"

coverageExcludedPackages := "org.zella.cards.Ranks;org.zella.cards.Suits;org.zella.combination.CombinationType"

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"

// https://mvnrepository.com/artifact/org.scalatest/scalatest_2.12
libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.4" % "test"

// https://mvnrepository.com/artifact/com.typesafe.play/play-json_2.12
libraryDependencies += "com.typesafe.play" % "play-json_2.12" % "2.6.7"


