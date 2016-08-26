// ---------------------------------------------------------------------------
// general settings
// ---------------------------------------------------------------------------

name := "MISE"
version := "0.1"
scalaVersion := "2.11.8"

mainClass in (Compile, run) := Some("dmf.stream.mutinf.MISECommandLineInterface")

// ---------------------------------------------------------------------------
// dependencies
// ---------------------------------------------------------------------------

// *** 2.10 => 2.11 migration
libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.3"

// *** ScalaTest
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

// *** Scala IO
// successor of scalax.io: http://jesseeichar.github.com/scala-io-doc/0.4.0/index.html#!/getting-started
// updated 09.05.2014 from 0.4.1
resolvers += "Java.net" at "http://download.java.net/maven/2/" 
libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.3"
libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.3"

// *** Apache commons math3 for binomial coefficients
libraryDependencies += "org.apache.commons" % "commons-math3" % "3.2"

// *** Logging
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0"
libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.6"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.1"

// ---------------------------------------------------------------------------
// compiler options
// ---------------------------------------------------------------------------

scalacOptions ++= Seq("-deprecation", "-language:implicitConversions", "-feature")

// ---------------------------------------------------------------------------
// fork options, cf. https://github.com/harrah/xsbt/wiki/Forking
// ---------------------------------------------------------------------------

fork in run := true

// prevents prefic "[info]" in stdout when forking:
outputStrategy := Some(StdoutOutput)

javaOptions in run += "-Xmx8G"


