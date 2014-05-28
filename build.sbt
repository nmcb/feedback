name := "emc-rna"

version := "1.12.4-SNAPSHOT"

scalaVersion := "2.11.0"

resolvers ++= Seq(
  "typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"
)

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "2.3.11" % "test",
  "junit"       % "junit"  % "4.11"   % "test"
)

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-Xlint",
  "-Ywarn-dead-code",
  "-language:_",
  "-target:jvm-1.7",
  "-encoding", "UTF-8"
)