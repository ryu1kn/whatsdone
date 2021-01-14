val gatlingVersion = "3.5.0"
val awsSdkVersion = "2.15.61"

lazy val root = (project in file("."))
  .enablePlugins(GatlingPlugin)
  .settings(
    inThisBuild(List(
      organization := "io.ryuichi",
      scalaVersion := "2.13.4",
      version      := "0.1.0-SNAPSHOT",
      scalacOptions := Seq(
        "-encoding",
        "UTF-8",
        "-target:jvm-1.8",
        "-deprecation",
        "-feature",
        "-unchecked",
        "-language:implicitConversions",
        "-language:postfixOps"
      ),
      resolvers ++= Seq(
        Resolver.bintrayIvyRepo("gatling", "sbt-plugins"),
        Resolver.jcenterRepo
      )
    )),
    name := "whatsdone-load-test",
    libraryDependencies ++= Seq(
      "software.amazon.awssdk" % "secretsmanager" % awsSdkVersion % Test,
      "software.amazon.awssdk" % "cognitoidentityprovider" % awsSdkVersion % Test,
      "org.scalatest" %% "scalatest" % "3.2.2" % Test,
      "io.gatling.highcharts" % "gatling-charts-highcharts" % gatlingVersion % Test,
      "io.gatling" % "gatling-test-framework" % gatlingVersion % Test
    )
  )
