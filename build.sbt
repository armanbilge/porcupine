ThisBuild / tlBaseVersion := "0.0"

ThisBuild / organization := "com.armanbilge"
ThisBuild / organizationName := "Arman Bilge"
ThisBuild / developers += tlGitHubDev("armanbilge", "Arman Bilge")
ThisBuild / startYear := Some(2023)

ThisBuild / tlSonatypeUseLegacyHost := false

ThisBuild / crossScalaVersions := Seq("3.3.0")

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

ThisBuild / githubWorkflowBuildPreamble ++= nativeBrewInstallWorkflowSteps.value
ThisBuild / nativeBrewInstallCond := Some("matrix.project == 'rootNative'")

ThisBuild / Test / testOptions += Tests.Argument("+l")

lazy val root = tlCrossRootProject.aggregate(
  core,
)

lazy val core = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .in(file("core"))
  .settings(
    name := "porcupine",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % "2.9.0",
      "org.typelevel" %%% "cats-effect" % "3.5.3",
      "co.fs2" %%% "fs2-core" % "3.7.0",
      "org.scodec" %%% "scodec-bits" % "1.1.37",
    ),
    Test / test := (Test / run).toTask("").value,
    Test / mainClass := Some("porcupine.PorcupineTest"),
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      "org.xerial" % "sqlite-jdbc" % "3.44.0.0",
    ),
    fork := true,
  )
  .jsSettings(
    Test / scalaJSUseMainModuleInitializer := true,
    Test / scalaJSUseTestModuleInitializer := false,
    Test / scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)),
    jsEnv := {
      import org.scalajs.jsenv.nodejs.NodeJSEnv
      new NodeJSEnv(NodeJSEnv.Config().withArgs(List("--enable-source-maps")))
    },
  )
  .nativeConfigure(_.enablePlugins(ScalaNativeBrewedConfigPlugin))
  .nativeSettings(
    nativeBrewFormulas += "sqlite",
    nativeConfig ~= { c => c.withLinkingOptions(c.linkingOptions :+ "-lsqlite3") },
  )
