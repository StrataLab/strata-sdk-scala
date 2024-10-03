package xyz.stratalab.sdk.servicekit

import cats.effect.IO
import co.topl.brambl.models.Indices
import munit.CatsEffectSuite
import xyz.stratalab.strata.servicekit.EasyApi
import xyz.stratalab.strata.servicekit.EasyApi.InitArgs

class EasyApiSpec extends CatsEffectSuite with BaseSpec {

  private def getFileName(name: String) = s"$TEST_DIR/$name"

  private val testArgs = InitArgs(
    dbFile = getFileName("easyApi.db"),
    keyFile = getFileName("easyApiKey.json"),
    mnemonicFile = getFileName("easyApiMnemonic.txt")
  )

  testDirectory.test("Initialize Easy API") { _ =>
    assertIO(
      for {
        apiFirst  <- EasyApi.initialize[IO]("password", testArgs)
        idxFirst  <- apiFirst.walletStateAlgebra.getCurrentIndicesForFunds("self", "default", None)
        _         <- apiFirst.walletStateAlgebra.updateWalletState("test", "test", None, None, Indices(1, 1, 2))
        apiSecond <- EasyApi.initialize[IO]("password", testArgs)
        idxSecond <- apiSecond.walletStateAlgebra.getCurrentIndicesForFunds("self", "default", None)
      } yield idxFirst.contains(Indices(1, 1, 1)) && idxSecond.contains(Indices(1, 1, 2)),
      true
    )
  }

}
