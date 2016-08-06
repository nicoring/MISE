// ---------------------------------------------------------------------------
// general settings
// ---------------------------------------------------------------------------

name := "MISE"

version := "0.1"

scalaVersion := "2.11.8"

// mainClass := Some("dmf.stream.mutinf.MISECommandLineInterface")

// ---------------------------------------------------------------------------
// dependencies
// ---------------------------------------------------------------------------

// 2.10 => 2.11 migration
libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.3"

// *** ScalaTest

//libraryDependencies += "org.scalatest" %% "scalatest" % "1.6.1" % "test"
libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"

libraryDependencies += "org.jfree" % "jfreechart" % "1.0.14"

libraryDependencies += "org.apache.xmlgraphics" % "batik-awt-util" % "1.7"

libraryDependencies += "org.apache.xmlgraphics" % "batik-util" % "1.7"

libraryDependencies += "org.apache.xmlgraphics" % "batik-dom" % "1.7"

libraryDependencies += "org.apache.xmlgraphics" % "batik-xml" % "1.7"

libraryDependencies += "org.apache.xmlgraphics" % "batik-svggen" % "1.7"

libraryDependencies += "org.encog" % "encog-core" % "3.1.0"

libraryDependencies += "com.thoughtworks.xstream" % "xstream" % "1.4.2"


// *** Scala IO
// successor of scalax.io: http://jesseeichar.github.com/scala-io-doc/0.4.0/index.html#!/getting-started
// updated 09.05.2014 from 0.4.1

resolvers += "Java.net" at "http://download.java.net/maven/2/" 

libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.3"

libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.3"

// *** Typesafe stuff

//resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
//resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/snapshots/"


// *** Apache commons math3 for binomial coefficients
libraryDependencies += "org.apache.commons" % "commons-math3" % "3.2"


// *** EJML, efficient linear algebra package (alternative to JBlas and much faster the LA in Apache's common math), updated 09.05.2014 from 0.21

libraryDependencies += "com.googlecode.efficient-java-matrix-library" % "ejml" % "0.24"


// *** Logging

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0"

// version 1.1.0 gives a "Potentially incompatible versions" warning for scala 2.10.3; use version 1.0.1 in case of trouble... ah was only an sbt bug, solved in 0.12.4

libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.6"

// libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.6"        // the simple backend must not be loaded when using logback

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.1"


// *** Joda Time, recommended to calculate difference of dates

libraryDependencies += "joda-time" % "joda-time" % "2.3"

libraryDependencies += "org.joda" % "joda-convert" % "1.6"


// ---------------------------------------------------------------------------
// compiler options
// ---------------------------------------------------------------------------

scalacOptions ++= Seq("-deprecation", "-language:implicitConversions") // "-feature"


// ---------------------------------------------------------------------------
// fork options, cf. https://github.com/harrah/xsbt/wiki/Forking
// ---------------------------------------------------------------------------

fork in run := true

// prevents prefic "[info]" in stdout when forking:
outputStrategy := Some(StdoutOutput)

javaOptions in run += "-Xmx8G"


