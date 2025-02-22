lazy val root = project
  .in(file("."))
  .settings(
    name := "scala3-example-project",
    description := "Example sbt project that compiles using Scala 3",
    version := "0.1.0",
    scalaVersion := "3.0.0",
    libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.2" % Test,
  )
