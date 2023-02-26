ThisBuild / tlBaseVersion := "0.0"

ThisBuild / organization := "com.armanbilge"
ThisBuild / organizationName := "Arman Bilge"
ThisBuild / developers += tlGitHubDev("armanbilge", "Arman Bilge")
ThisBuild / startYear := Some(2023)

ThisBuild / tlSonatypeUseLegacyHost := false

ThisBuild / crossScalaVersions := Seq("3.2.2")

ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17"))
ThisBuild / tlJdkRelease := Some(8)

ThisBuild / githubWorkflowBuildPreamble ++= Seq(
  WorkflowStep.Use(
    UseRef.Public("actions", "setup-node", "v3"),
    name = Some("Setup NodeJS v18 LTS"),
    params = Map("node-version" -> "18", "cache" -> "npm"),
    cond = Some("matrix.project == 'rootJS'"),
  ),
  WorkflowStep.Run(
    List("npm install"),
    cond = Some("matrix.project == 'rootJS'"),
  ),
)

ThisBuild / Test / testOptions += Tests.Argument("+l")

val commonJvmSettings = Seq(
  fork := true,
)

lazy val root = tlCrossRootProject.aggregate(
  core,
)

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .in(file("core"))
  .settings(
    name := "porcupine",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % "2.9.0",
      "org.typelevel" %%% "cats-effect" % "3.4.8",
      "co.fs2" %%% "fs2-core" % "3.6.1",
    ),
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      "org.xerial" %%% "sqlite-jdbc" % "3.41.0.0",
    ),
  )
