name := "ProbabilisticModels"

version := "1.0"

scalaVersion := "2.11.8"

lazy val root = (project in file("."))
  .settings (scalacOptions ++= Seq(
    "-feature",
    "-language:existentials",
    "-deprecation",
    "-language:postfixOps"
  ))
  .settings(libraryDependencies ++= Seq(
    "org.apache.commons" % "commons-math3" % "3.6",
    "org.scalactic" %% "scalactic" % "2.2.6",
    "org.scalatest" %% "scalatest" % "2.2.6" % "test"
  ))
  // Copy all managed dependencies to \lib_managed directory
  .settings(retrieveManaged := true)
