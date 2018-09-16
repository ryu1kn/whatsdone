import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.3"
  lazy val gatlingHighChart = "io.gatling.highcharts" % "gatling-charts-highcharts" % "2.3.1"
  lazy val gatling = "io.gatling" % "gatling-test-framework" % "2.3.1"
}
