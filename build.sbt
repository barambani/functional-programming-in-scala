lazy val commonSettings = Seq (
  scalaVersion := "2.11.8"
)

lazy val simpleCqrsScala = (project in file(".")).settings(commonSettings: _*)

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.6"
)

initialCommands in console := """
  |import java.util.UUID
  |import scalaz._
  |import Scalaz._
  |import ListExercise._
""".stripMargin

logLevel := Level.Info
