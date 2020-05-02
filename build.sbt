name := "AlgReal"
version := "1.0"
scalaVersion := "2.12.8"

scalacOptions := Seq("-feature", "-unchecked", "-deprecation")
scalacOptions += "-Ypartial-unification"

lazy val libDependencies = Seq(
  "org.typelevel" %% "cats-core" % "2.0.0"
)

lazy val testLibraryDependencies = Seq(
  "org.scalatest" %% "scalatest" % "3.1.0" % Test,
)

libraryDependencies ++= libDependencies ++ testLibraryDependencies
