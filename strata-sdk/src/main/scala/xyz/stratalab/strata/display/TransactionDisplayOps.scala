package xyz.stratalab.sdk.display

import xyz.stratalab.sdk.display.DisplayOps.DisplayTOps
import xyz.stratalab.sdk.models.TransactionId
import xyz.stratalab.sdk.models.transaction.IoTransaction
import xyz.stratalab.sdk.syntax.ioTransactionAsTransactionSyntaxOps
import xyz.stratalab.sdk.utils.Encoding

trait TransactionDisplayOps {

  implicit val transactionIdDisplay: DisplayOps[TransactionId] = (id: TransactionId) =>
    Encoding.encodeToBase58(id.value.toByteArray())

  implicit val transactionDisplay: DisplayOps[IoTransaction] = (tx: IoTransaction) =>
    s"""
${padLabel("TransactionId")}${tx.transactionId.getOrElse(tx.computeId).display}

Group Policies
==============
${tx.groupPolicies.map(gp => gp.display).mkString("\n-----------\n")}

Series Policies
===============
${tx.seriesPolicies.map(sp => sp.display).mkString("\n-----------\n")}

Asset Minting Statements
========================
${tx.mintingStatements.map(ams => ams.display).mkString("\n-----------\n")}

Asset Merging Statements
========================
${tx.mergingStatements.map(ams => ams.display).mkString("\n-----------\n")}

Inputs
======
${if (tx.inputs.isEmpty) ("No inputs")
      else tx.inputs.map(stxo => stxo.display).mkString("\n-----------\n")}

Outputs
=======
${if (tx.outputs.isEmpty) ("No outputs")
      else tx.outputs.map(utxo => utxo.display).mkString("\n-----------\n")}

Datum
=====
${padLabel("Value")}${Encoding.encodeToBase58(tx.datum.event.metadata.value.toByteArray())}
"""
}
