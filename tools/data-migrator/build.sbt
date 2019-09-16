name := "data-migrator"
version := "0.0.1-SNAPSHOT"

scalaVersion := "2.13.0"

scalacOptions ++= Seq(
  "-encoding", "UTF-8",   // source files are in UTF-8
  "-deprecation",         // warn about use of deprecated APIs
  "-unchecked",           // warn about unchecked type parameters
  "-feature",             // warn about misused language features
  "-language:higherKinds",// allow higher kinded types without `import scala.language.higherKinds`
  "-Xlint"                // enable handy linter warnings
)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.0.0",
  "org.typelevel" %% "cats-effect" % "2.0.0",
  "org.scalatest" %% "scalatest" % "3.0.8" % "test",
  "software.amazon.awssdk" % "dynamodb" % "2.5.63"
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")
