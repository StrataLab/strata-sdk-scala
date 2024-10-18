package org.plasmalabs.crypto.hash

import org.plasmalabs.crypto.hash.digest.Digest

import java.security.MessageDigest

/**
 * SHA2-* hashing scheme
 *
 * @tparam D the sized digest type with an implicit Digest implementation
 */
abstract class ShaHash[D: Digest](val algorithmName: String) extends Hash[Sha, D] {

  override def hash(prefix: Option[Byte], messages: Message*): D =
    Digest[D]
      .from(
        MessageDigest
          .getInstance(algorithmName)
          .digest(
            messages.foldLeft(
              prefix
                .map(Array[Byte](_))
                .getOrElse(Array[Byte]())
            )(_ ++ _)
          )
      )
      .valueOr(err => throw new Error(s"Sha hash with digest size ${Digest[D].size} was invalid! $err"))
}
