lazy val feedback = (project in file(".")).
    settings(
      name := "feedback",
      version := "1.12.4-SNAPSHOT",
      scalaVersion := "2.11.6",
      scalacOptions ++= Seq(
        "-unchecked",
        "-deprecation",
        "-Xlint",
        "-Ywarn-dead-code",
        "-language:_",
        "-target:jvm-1.8",
        "-encoding", "UTF-8"
      ))

initialCommands in console :=
  """
    | import emc.rna._
  """.stripMargin