inThisBuild(
  List(
    organization := "org.plasmalabs",
    homepage := Some(url("https://github.com/PlasmaLaboratories/plasma-sdk-scala")),
    licenses := Seq("MPL2.0" -> url("https://www.mozilla.org/en-US/MPL/2.0/")),
    scalaVersion := "2.13.13",
    testFrameworks += TestFrameworks.MUnit
  )
)

lazy val commonScalacOptions = Seq(
  "-deprecation",
  "-feature",
  "-language:higherKinds",
  "-language:postfixOps",
  "-unchecked",
  "-Ywarn-unused",
  "-Yrangepos"
)

lazy val commonSettings = Seq(
  fork := true,
  scalacOptions ++= commonScalacOptions,
  semanticdbEnabled := true, // enable SemanticDB for Scalafix
  Compile / unmanagedSourceDirectories += {
    val sourceDir = (Compile / sourceDirectory).value
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, n)) if n >= 13 => sourceDir / "scala-2.13+"
      case _                       => sourceDir / "scala-2.12-"
    }
  },
  Test / testOptions ++= Seq(
    Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "2"),
    Tests.Argument(TestFrameworks.ScalaTest, "-f", "sbttest.log", "-oDGG", "-u", "target/test-reports")
  ),
  resolvers ++= Seq(
    "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/",
    "Sonatype Staging" at "https://s01.oss.sonatype.org/content/repositories/staging",
    "Sonatype Snapshots" at "https://s01.oss.sonatype.org/content/repositories/snapshots/",
    "Sonatype Releases s01" at "https://s01.oss.sonatype.org/content/repositories/releases/",
    "Bintray" at "https://jcenter.bintray.com/",
    "jitpack" at "https://jitpack.io"
  ),
  addCompilerPlugin("org.typelevel" % "kind-projector"     % "0.13.3" cross CrossVersion.full),
  addCompilerPlugin("com.olegpy"   %% "better-monadic-for" % "0.3.1"),
  testFrameworks += TestFrameworks.MUnit
)

lazy val publishSettings = Seq(
  homepage := Some(url("https://github.com/PlasmaLaboratories/plasma-sdk-scala")),
  ThisBuild / sonatypeCredentialHost := "s01.oss.sonatype.org",
  sonatypeRepository := "https://s01.oss.sonatype.org/service/local",
  Test / publishArtifact := false,
  pomIncludeRepository := { _ => false },
  pomExtra :=
    <developers>
      <developer>
        <id>scasplte2</id>
        <name>James Aman</name>
      </developer>
      <developer>
        <id>mgrand</id>
        <name>Mark Grand</name>
      </developer>
      <developer>
        <id>DiademShoukralla</id>
        <name>Diadem Shoukralla</name>
      </developer>
      <developer>
        <id>mundacho</id>
        <name>Edmundo Lopez Bobeda</name>
      </developer>
    </developers>
)

lazy val macroAnnotationsSettings =
  Seq(
    scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, v)) if v >= 13 =>
          Seq(
            "-Ymacro-annotations"
          )
        case _ =>
          Nil
      }
    }
  )

lazy val crypto = project
  .in(file("crypto"))
  .settings(
    name := "crypto",
    commonSettings,
    publishSettings,
    Test / publishArtifact := true,
    libraryDependencies ++=
      Dependencies.Crypto.sources ++
      Dependencies.Crypto.tests,
    macroAnnotationsSettings
  )

lazy val quivr4s = project
  .in(file("quivr4s"))
  .settings(
    name := "quivr4s",
    commonSettings,
    publishSettings,
    Test / publishArtifact := true,
    Test / parallelExecution := false,
    libraryDependencies ++=
      Dependencies.Quivr4s.sources ++
        Dependencies.Quivr4s.tests
  )
  .dependsOn(crypto)

lazy val plasmaSdk = project
  .in(file("plasma-sdk"))
  .settings(
    name := "plasma-sdk",
    commonSettings,
    publishSettings,
    Test / publishArtifact := true,
    Test / parallelExecution := false,
    libraryDependencies ++=
      Dependencies.PlasmaSdk.sources ++
      Dependencies.PlasmaSdk.tests
  )
  .dependsOn(quivr4s % "compile->compile;test->test")

lazy val serviceKit = project
  .in(file("service-kit"))
  .settings(
    name := "service-kit",
    commonSettings,
    publishSettings,
    Test / publishArtifact := true,
    Test / parallelExecution := false,
    libraryDependencies ++=
      Dependencies.ServiceKit.sources ++
        Dependencies.ServiceKit.tests
  )
  .dependsOn(plasmaSdk)

lazy val integration = project
  .in(file("integration"))
  .settings(
    name := "integration",
    publish / skip := true,
    libraryDependencies ++= Dependencies.IntegrationTests.sources ++ Dependencies.IntegrationTests.tests
  )
  .dependsOn(plasmaSdk)

val DocumentationRoot = file("documentation") / "static" / "scaladoc" / "current"

lazy val plasma = project
  .in(file("."))
  .settings(
    moduleName := "plasma",
    commonSettings,
    publish / skip := true,
    // Currently excluding crypto since there are issues due to the use of macro annotations
    ScalaUnidoc / unidoc / unidocProjectFilter := inAnyProject -- inProjects(crypto),
    ScalaUnidoc / unidoc / target := DocumentationRoot,
  )
  .enablePlugins(ReproducibleBuildsPlugin, ScalaUnidocPlugin)
  .aggregate(
    crypto,
    plasmaSdk,
    serviceKit,
    quivr4s
  )

addCommandAlias("checkPR", s"; scalafixAll --check; scalafmtCheckAll; +coverage; +test; +coverageReport")
addCommandAlias("preparePR", s"; scalafixAll; scalafmtAll; +test; unidoc")
addCommandAlias("checkPRTestQuick", s"; scalafixAll --check; scalafmtCheckAll; testQuick")
