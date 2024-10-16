package xyz.stratalab.sdk.syntax

import xyz.stratalab.sdk.common.ContainsEvidence
import xyz.stratalab.sdk.common.ContainsImmutable
import xyz.stratalab.sdk.common.ContainsSignable
import xyz.stratalab.sdk.common.ContainsSignable.instances.ioTransactionSignable
import xyz.stratalab.sdk.models.TransactionId
import xyz.stratalab.sdk.models.common.ImmutableBytes
import xyz.stratalab.sdk.models.transaction.IoTransaction

import scala.language.implicitConversions

trait TransactionSyntax {

  implicit def ioTransactionAsTransactionSyntaxOps(transaction: IoTransaction): TransactionSyntaxOps =
    new TransactionSyntaxOps(transaction)
}

class TransactionSyntaxOps(val transaction: IoTransaction) extends AnyVal {

  /**
   * The ID of this transaction.  If an ID was pre-computed and saved in the Transaction, it is restored.
   * Otherwise, a new ID is computed (but not saved in the Transaction).
   */
  def id: TransactionId =
    transaction.transactionId.getOrElse(computeId)

  /**
   * Computes what the ID _should_ be for this Transaction.
   */
  def computeId: TransactionId = {
    import TransactionSyntaxOps._
    val signableBytes = ContainsSignable[IoTransaction].signableBytes(transaction)
    val immutable = ImmutableBytes(signableBytes.value)
    val evidence = ContainsEvidence[ImmutableBytes].sizedEvidence(immutable)
    TransactionId(evidence.digest.value)
  }

  /**
   * Compute a new ID and return a copy of this Transaction with the new ID embedded.
   * Any previous value will be overwritten in the new copy.
   */
  def embedId: IoTransaction =
    transaction.copy(transactionId = Some(computeId))

  /**
   * Returns true if this Transaction contains a valid embedded ID.
   */
  def containsValidId: Boolean =
    transaction.transactionId.contains(computeId)
}

object TransactionSyntaxOps {
  implicit private val immutableContainsImmutable: ContainsImmutable[ImmutableBytes] = identity
}
