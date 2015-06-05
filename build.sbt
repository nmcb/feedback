lazy val feedback = (project in file(".")).
    settings(
      name := "feedback",
      version := "1.12.4-SNAPSHOT",
      scalaVersion := "2.11.6",
      libraryDependencies ++= Seq(
        "org.yaml" % "snakeyaml" % "1.15",
        "org.specs2" %% "specs2" % "2.3.11" % "test",
        "junit" % "junit" % "4.11" % "test"
      ),
      scalacOptions ++= Seq(
        "-unchecked",
        "-deprecation",
        "-Xlint",
        "-Ywarn-dead-code",
        "-language:_",
        "-target:jvm-1.8",
        "-encoding", "UTF-8"
      ),
      resolvers ++= Seq(
        "typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"
      ))

initialCommands in console :=
  """
    | import zalando.conf.yaml._
  """.stripMargin