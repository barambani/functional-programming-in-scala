lazy val commonSettings = Seq (
  scalaVersion := "2.11.8"
)

lazy val fpInScala = (project in file(".")).settings(commonSettings: _*)

initialCommands in console := """
  |import ListExercise._
  |import TreeExercise._
  |import OptionExercise._ 
  |import OptionExercise.Option._
  |import EitherExercise._
""".stripMargin

scalacOptions ++= Seq (
  "-feature",
  "-deprecation",
  "-target:jvm-1.8"
)

logLevel := Level.Info
