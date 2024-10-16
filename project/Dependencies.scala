import Dependencies.Versions._
import sbt._

object Dependencies {

  object Versions {
    val catsCoreVersion = "2.10.0"
    val circeVersion = "0.14.6"
    val protobufSpecsVersion = "0.1.0+4-d8600bac-SNAPSHOT"
    val mUnitTeVersion = "0.7.29"
    val btcVersion = "1.9.9"
    val btcVersionZmq = "1.9.8"
    val scodecBitsVersion = "1.2.1"
  }

  val catsSlf4j: ModuleID =
    "org.typelevel" %% "log4cats-slf4j" % "2.4.0"

  val fs2Io: ModuleID = "co.fs2" %% "fs2-io" % "3.10.2"

  val circe: Seq[ModuleID] = Seq(
    "io.circe" %% "circe-core"    % circeVersion,
    "io.circe" %% "circe-parser"  % circeVersion,
    "io.circe" %% "circe-generic" % circeVersion
  )

  val scalacheck: Seq[ModuleID] = Seq(
    "org.scalacheck"    %% "scalacheck"      % "1.17.0",
    "org.scalatestplus" %% "scalacheck-1-16" % "3.2.14.0"
  )

  def scalamock(v: Option[(Long, Long)]): Seq[ModuleID] =
    Seq(v match {
      case Some((2, 13)) =>
        "org.scalamock" %% "scalamock" % "5.2.0"
      case _ =>
        "eu.monniot" %% "scala3mock" % "0.1.1"
    })

  def scalatest(v: Option[(Long, Long)]): Seq[ModuleID] =
    Seq(
      "org.scalatest" %% "scalatest" % "3.2.18",
      v match {
        case Some((2, 13)) =>
          "com.ironcorelabs" %% "cats-scalatest" % "3.1.1"
        case _ =>
          "com.ironcorelabs" %% "cats-scalatest" % "4.0.0"
      },
      "org.typelevel" %% "cats-effect-testing-scalatest" % "1.5.0"
    )

  val mUnitTest: Seq[ModuleID] = Seq(
    "org.scalameta" %% "munit"                   % mUnitTeVersion,
    "org.scalameta" %% "munit-scalacheck"        % mUnitTeVersion,
    "org.typelevel" %% "munit-cats-effect-3"     % "1.0.7",
    "org.typelevel" %% "scalacheck-effect-munit" % "1.0.4"
  )

  val cats: Seq[ModuleID] = Seq(
    "org.typelevel" %% "cats-core"   % catsCoreVersion,
    "org.typelevel" %% "mouse"       % "1.2.3",
    "org.typelevel" %% "cats-free"   % catsCoreVersion,
    "org.typelevel" %% "cats-effect" % "3.5.4"
  )

  val protobufSpecs: Seq[ModuleID] = Seq(
    "xyz.stratalab" %% "protobuf-fs2" % protobufSpecsVersion
  )

  val sqlite: Seq[ModuleID] = Seq(
    "org.xerial" % "sqlite-jdbc" % "3.45.2.0"
  )

  val grpcNetty = "io.grpc" % "grpc-netty" % "1.62.2"

  /**
   * There is no bitcoin-s artifacts for scala3, 2.13 is used, but transitive makes conflicts with scodec.
   * scodec-bits_3 was excluded in all projects
   * https://github.com/bitcoin-s/bitcoin-s/issues/5672
   */
  val btc: Seq[ModuleID] = Seq(
    "org.bitcoin-s" %% "bitcoin-s-core"         % btcVersion cross CrossVersion.for3Use2_13,
    "org.bitcoin-s" %% "bitcoin-s-zmq"          % btcVersionZmq cross CrossVersion.for3Use2_13,
    "org.bitcoin-s" %% "bitcoin-s-bitcoind-rpc" % btcVersion cross CrossVersion.for3Use2_13
  )

  val scodec3ExlusionRule = ExclusionRule("org.scodec", "scodec-bits_3")

  object Crypto {

    lazy val sources: Seq[ModuleID] =
      Seq("org.bouncycastle" % "bcprov-jdk18on" % "1.77") ++
      circe ++
      cats

    def tests(v: Option[(Long, Long)]): Seq[ModuleID] =
      (
        scalatest(v) ++
          scalamock(v) ++
          scalacheck
      )
        .map(_ % Test)
  }

  object StrataSdk {

    lazy val sources: Seq[ModuleID] = Dependencies.protobufSpecs ++ btc :+ grpcNetty

    def tests(v: Option[(Long, Long)]): Seq[ModuleID] =
      (
        mUnitTest ++
          scalamock(v)
      ).map(_ % Test)
  }

  object ServiceKit {

    lazy val sources: Seq[ModuleID] = sqlite

    lazy val tests: Seq[ModuleID] = (
      mUnitTest ++ sqlite
    ).map(_ % Test)
  }

  object IntegrationTests {

    lazy val sources: Seq[ModuleID] =
      Crypto.sources ++ StrataSdk.sources ++ ServiceKit.sources :+ fs2Io
    lazy val tests: Seq[ModuleID] = (sources ++ mUnitTest).map(_ % Test)
  }

  object Quivr4s {

    lazy val sources: Seq[ModuleID] = Dependencies.protobufSpecs

    lazy val tests: Seq[ModuleID] = (
      mUnitTest
    ).map(_ % Test)
  }
}
