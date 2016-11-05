lazy val commonSettings = Seq (
  scalaVersion := "2.12.0"
)

lazy val fpInScala = (project in file(".")).settings(commonSettings: _*)

initialCommands in console := """
  |import ListExercise._
  |import TreeExercise._
  |import OptionExercise._
""".stripMargin

scalacOptions ++= Seq (
  "-feature",
  "-deprecation",
  "-target:jvm-1.8"
)

logLevel := Level.Info
