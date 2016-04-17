lazy val Benchmark = config("bench") extend Test

lazy val treap = Project(
  id = "treap",
  base = file("."),
  settings = Defaults.coreDefaultSettings ++ Seq(
    name := "treap",
    organization := "org.saegesser",
    version := "0.0.1",
    scalaVersion := "2.11.8",
    crossScalaVersions := List("2.11.8", "2.10.5"),
    scalacOptions in ThisBuild ++= Seq(
      "-feature",
      "-deprecation",
      "-Yno-adapted-args",
      "-Ywarn-value-discard",
      "-Ywarn-numeric-widen",
      "-Ywarn-dead-code",
      "-Xlint",
      "-Xfatal-warnings",
      "-unchecked",
      // "-Ymacro-debug-lite",
      "-language:implicitConversions",
      "-language:experimental.macros"
    ),
    libraryDependencies ++= Seq(
      "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2",
      "ch.qos.logback"             %  "logback-core"        % "1.1.3",
      "org.scalatest"              %% "scalatest"           % "2.2.4"        % "test",
      "org.scalacheck"             %% "scalacheck"          % "1.13.0"       % "test",
      "org.scalamock"              %% "scalamock-scalatest-support" % "3.2"  % "test",
      "com.storm-enroute"          %% "scalameter"          % "0.7"          % "bench"
    ),
    testFrameworks in Benchmark := Seq(new TestFramework("org.scalameter.ScalaMeterFramework")),
    logBuffered := false,
    parallelExecution in Test := false,
    parallelExecution in Benchmark := false,
    initialCommands := """
      import treaps._
    """
  )
) configs(
  Benchmark
) settings (
  inConfig(Benchmark)(Defaults.testSettings):_*
)
