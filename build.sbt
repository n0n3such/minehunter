name := """minehunter"""

version := "1.0"

scalaVersion := "2.11.6"

val scalazVersion = "7.1.11"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % scalazVersion
)

scalacOptions += "-feature"

initialCommands in console := "import scalaz._, Scalaz._"


