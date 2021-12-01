val scala3Version = "3.1.0"

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / scalacOptions ++=
  Seq(
    "-deprecation",
    "-feature",
    "-language:implicitConversions",
    "-unchecked",
    "-Xfatal-warnings",
    "-Yexplicit-nulls", // experimental (I've seen it cause issues with circe)
    "-Ykind-projector",
    "-Ysafe-init", // experimental (I've seen it cause issues with circe)
  ) ++ Seq("-rewrite", "-indent") ++ Seq("-source", "future")

lazy val root = project
  .in(file("."))
  .settings(
    name := "mastermind",
    version := "0.1.0",
    scalaVersion := scala3Version,
    libraryDependencies ++=
      Seq(
        "org.scalameta" %% "munit" % "0.7.29" % Test,
        "org.scalameta" %% "munit-scalacheck" % "0.7.29" % Test
      )
  )
