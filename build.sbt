name := "FocusedReading"
scalaVersion := "2.11.8"
version := "0.1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "com.typesafe" % "config" % "1.2.1",
  "commons-io" % "commons-io" % "2.4",
  "jline" % "jline" % "2.12.1",
  // logging
  "ch.qos.logback" %  "logback-classic" % "1.1.7",
  "com.typesafe.scala-logging" %%  "scala-logging" % "3.4.0",
  "org.scala-graph" %% "graph-core" % "1.11.3",
  "org.scalanlp" %% "breeze" % "0.13",
  "org.scalanlp" %% "breeze-natives" % "0.13",
  "org.scalanlp" %% "breeze-viz" % "0.13",
  "org.jfree" % "jfreechart" % "1.0.19",
  "org.clulab" %% "sarsamora" % "0.0.0-SNAPSHOT",
  "org.clulab" %% "processors-main" % "6.0.1",
  "org.clulab" %% "processors-corenlp" % "6.0.1",
  "org.clulab" %% "processors-models" % "6.0.1",
  "org.apache.lucene" % "lucene-core" % "5.3.1",
  "org.apache.lucene" % "lucene-analyzers-common" % "5.3.1",
  "org.apache.lucene" % "lucene-queryparser" % "5.3.1",
  "org.clulab" %% "reach-main" % "1.3.3-SNAPSHOT"
)
