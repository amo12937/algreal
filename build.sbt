name := "AlgReal"
version := "1.0"
scalaVersion := "2.12.8"

scalacOptions += "-feature"

lazy val testLibraryDependencies = Seq(
  "org.scalatest" %% "scalatest" % "3.1.0" % Test,
)

libraryDependencies ++= testLibraryDependencies
