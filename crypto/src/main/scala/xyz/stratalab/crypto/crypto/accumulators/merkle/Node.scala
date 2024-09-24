package xyz.stratalab.crypto.accumulators.merkle

import xyz.stratalab.crypto.accumulators.LeafData
import xyz.stratalab.crypto.hash.Hash
import xyz.stratalab.crypto.hash.digest.Digest
import xyz.stratalab.crypto.hash.digest.implicits._

/* Forked from https://github.com/input-output-hk/scrypto */

sealed abstract class Node[D: Digest] {
  def hash: D
}

/**
 * Internal node in Merkle tree
 *
 * @param left  - left child. always non-empty
 * @param right - right child. can be emptyNode
 */
case class InternalNode[H, D: Digest](left: Node[D], right: Option[Node[D]])(implicit h: Hash[H, D]) extends Node[D] {

  override lazy val hash: D =
    h.hash(
      MerkleTree.InternalNodePrefix,
      left.hash.bytes ++ right.map(_.hash.bytes).getOrElse(Array[Byte]())
    )

}

/**
 * Merkle tree leaf
 *
 * @param data - leaf data.
 */
case class Leaf[H, D: Digest](data: LeafData)(implicit h: Hash[H, D]) extends Node[D] {
  override lazy val hash: D = h.hash(MerkleTree.LeafPrefix, data.value)
}
