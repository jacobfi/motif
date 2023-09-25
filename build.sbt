name := "motif"

version := "0.1"

ThisBuild / scalaVersion := "3.3.1"
ThisBuild / scalacOptions += "-deprecation"

lazy val core = project
  .settings(
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0"
  )

lazy val java = project
  .dependsOn(core)
  .settings(
    assembly / assemblyJarName := "motifc.jar",
    assembly / mainClass := Some("io.motif.midi.Main")
  )
