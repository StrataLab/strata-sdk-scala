package xyz.stratalab.sdk.display

import xyz.stratalab.sdk.display.DisplayOps.DisplayTOps
import xyz.stratalab.sdk.models.TransactionOutputAddress
import xyz.stratalab.sdk.models.transaction.SpentTransactionOutput
import xyz.stratalab.sdk.utils.Encoding
import xyz.stratalab.indexer.services.Txo

trait StxoDisplayOps {

  implicit val stxoDisplay: DisplayOps[SpentTransactionOutput] = (stxo: SpentTransactionOutput) =>
    Seq(
      padLabel("TxoAddress") + stxo.address.display,
      padLabel("Attestation") + "Not implemented",
      stxo.value.value.display
    ).mkString("\n")

  implicit val txoAddressDisplay: DisplayOps[TransactionOutputAddress] = (txoAddress: TransactionOutputAddress) =>
    s"${Encoding.encodeToBase58(txoAddress.id.value.toByteArray())}#${txoAddress.index}"

  implicit val txoDisplay: DisplayOps[Txo] = (txo: Txo) =>
    Seq(
      padLabel("TxoAddress") + txo.outputAddress.display,
      padLabel("LockAddress") + txo.transactionOutput.address.display,
      txo.transactionOutput.value.value.display
    ).mkString("\n")

}
