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
      "software.amazon.awssdk" % "secretsmanager" % "2.15.61" % Test,
      "software.amazon.awssdk" % "cognitoidentityprovider" % "2.15.61",
      "org.scalatest" %% "scalatest" % "3.0.5" % Test,
      "io.gatling.highcharts" % "gatling-charts-highcharts" % "2.3.1" % Test,
      "io.gatling" % "gatling-test-framework" % "2.3.1" % Test
    )
  )
