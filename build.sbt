ThisBuild / tlBaseVersion := "0.1"
ThisBuild / organization := "org.polyvariant"
ThisBuild / organizationName := "Polyvariant"
ThisBuild / startYear := Some(2026)
ThisBuild / licenses := Seq(License.Apache2)
ThisBuild / developers := List(tlGitHubDev("kubukoz", "Jakub Kozłowski"))

ThisBuild / githubWorkflowPublishTargetBranches := Seq(
  RefPredicate.Equals(Ref.Branch("main")),
  RefPredicate.StartsWith(Ref.Tag("v")),
)

ThisBuild / scalaVersion := "3.9.0"
ThisBuild / tlJdkRelease := Some(17)
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17"))
ThisBuild / tlFatalWarnings := false

ThisBuild / mergifyStewardConfig ~= (_.map(_.withMergeMinors(true)))

val hearthVersion = "0.4.2"
val kindlingsVersion = "0.3.2"
val http4sVersion = "0.23.38"
val circeVersion = "0.14.16"

lazy val core = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(
    name := "jev4s",
    scalacOptions ++= Seq(
      "-no-indent",
      "-Wunused:all",
    ),
    addCompilerPlugin("com.kubuszok" %% "hearth-cross-quotes" % hearthVersion),
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-effect" % "3.7.1",
      "org.typelevel" %%% "cats-free" % "2.13.0",
      "org.http4s" %%% "http4s-client" % http4sVersion,
      "org.http4s" %%% "http4s-circe" % http4sVersion,
      "io.circe" %%% "circe-core" % circeVersion,
      "com.kubuszok" %%% "hearth" % hearthVersion,
      "com.kubuszok" %%% "kindlings-derivation-commons" % kindlingsVersion,
      "com.kubuszok" %%% "kindlings-circe-derivation" % kindlingsVersion,
      "org.typelevel" %%% "munit-cats-effect" % "2.2.1" % Test,
      "org.http4s" %%% "http4s-ember-client" % http4sVersion % Test,
      "io.circe" %%% "circe-parser" % circeVersion % Test,
    ),
  )

lazy val root = tlCrossRootProject.aggregate(core)
