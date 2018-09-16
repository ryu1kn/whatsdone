import Dependencies._

lazy val root = (project in file("."))
  .enablePlugins(GatlingPlugin)
  .settings(
    inThisBuild(List(
      organization := "io.ryuichi",
      scalaVersion := "2.12.6",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "whatsdone-load-test",
    libraryDependencies ++= Seq(
      gatlingHighChart,
      gatling,
      scalaTest % Test
    )
  )
