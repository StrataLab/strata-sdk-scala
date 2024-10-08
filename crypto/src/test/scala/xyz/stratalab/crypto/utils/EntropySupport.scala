package xyz.stratalab.crypto.utils

import cats.Eq
import xyz.stratalab.crypto.generation.mnemonic.Entropy
import org.scalacheck.Arbitrary

trait EntropySupport {

  implicit val arbitraryEntropy: Arbitrary[Entropy] =
    Arbitrary.apply[Entropy](Arbitrary.arbUuid.arbitrary.map(Entropy.fromUuid))

  implicit val entropyEq: Eq[Entropy] = (a, b) => a.value == b.value
}

object EntropySupport extends EntropySupport
