package co.topl.brambl.common

import co.topl.brambl.common.ContainsSignable.ContainsSignableTOps
import co.topl.brambl.common.ContainsSignable.instances.ioTransactionSignable
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.syntax.ioTransactionAsTransactionSyntaxOps
import co.topl.brambl.utils.Encoding

class TransactionCodecVectorsSpec extends munit.FunSuite {

  import TransactionCodecVectorsSpec._

  vectors.zipWithIndex.foreach { case (vector, index) =>
    test(s"Vector $index should result in correct signable+id") {
      val txBytes = Encoding.decodeFromHex(vector.txHex).toOption.get
      val tx = IoTransaction.parseFrom(txBytes)
      val signable = tx.signable.value
      assertEquals(Encoding.encodeToHex(signable.toByteArray), vector.txSignableHex)
      val id = tx.computeId
      assertEquals(Encoding.encodeToBase58(id.value.toByteArray), vector.txId)
    }

  }
}

object TransactionCodecVectorsSpec {

  val vectors: List[TestVector] = List(
    TestVector(
      "1a060a040a002200",
      "0000",
      "BhHbw2zXrJGgRW9YpKQV4c6sXfSwChXeYrRjW1aCQqRF"
    ),
    TestVector(
      "0a360a2422220a207b4ffd7c46c3884c6095e58a2eb4b28b610d6c3fd5a3297831f828a443d466a012040a020a001a080a060a040a0201f412340a28080510321a220a207b40522d25601601b7c859f735195500cc906183ae108dbc6e8a33b672bae97c12080a060a040a0201f41a060a040a002200",
      "00000000696f5f7472616e73616374696f6e5f33327b4ffd7c46c3884c6095e58a2eb4b28b610d6c3fd5a3297831f828a443d466a00001f4000532626f785f6c6f636b5f33327b40522d25601601b7c859f735195500cc906183ae108dbc6e8a33b672bae97c01f40000",
      "DeXDSTN9JCDb6RAvL3iGaX68fFtEm4gHFYtYMT6BEWwN"
    ),
    TestVector(
      "0a3d0a2b0808104318d90222220a2082d4b6b33397ec74b6394284db2f2be79a6a950c3f0863347a79d2e2b92830d912040a020a001a0812060a040a0201f412340a28080510321a220a20897af2e0eb81f85365ff98f6578516973d96b0109fbc63c4ed8c7ecf1d6e751412080a060a040a0201f41a3c0a3a0a1408b11210ffffffffffffffff7f18fba1caf9a13222220a209d252954ade1c5909d397d97db968db897c8e663d94594ff4d7567fde9e6efc0",
      "0008430159696f5f7472616e73616374696f6e5f333282d4b6b33397ec74b6394284db2f2be79a6a950c3f0863347a79d2e2b92830d90001f4ff000532626f785f6c6f636b5f3332897af2e0eb81f85365ff98f6578516973d96b0109fbc63c4ed8c7ecf1d6e751401f409317fffffffffffffff9d252954ade1c5909d397d97db968db897c8e663d94594ff4d7567fde9e6efc0",
      "FvhubX87rvQvcMiUAVtiowyLaNGh2atx8P8cAeFWj5h7"
    ),
    TestVector(
      "0afd010a2b0808104318d90222220a205a11e2d3c75cb12f574fcaed074b2823b0aa331aba52440dc642689d79f4704012040a020a001ac7011ac4010a220a20314348ed39986e8d236adef6e7add19bec15c79b07fdf85c0a09982eb3c29cca12220a206866145571c2f9c2b8a3b52baf954f980c660a64ea1ed6ef16fd582ccf60d5e41a0a0a087fffffffffffffff22220a20c18b909bd6e69f35f770a46620bc09cbd5415e02a66e64189e2edf9bf91f683d2a220a20220ea05824fabed0821744ba6fd6f41b6f9804e40f4d527d544619c60d103f86300138014a220a2015332967ea30f22d40820a20c40f234ee5c9b38e10047f967322a12d20a2ee9e12340a28080510321a220a20495be2da0ca6024ddd25587dbd53f947a85f36ee2755bf4f378373d2f81ae57412080a060a040a0201f41a3c0a3a0a1408b11210ffffffffffffffff7f189e82b781a23222220a200fc364a34151d65147f5ad71e868d3bf0d5519b4e457353162c7406f8fd5256e",
      "0008430159696f5f7472616e73616374696f6e5f33325a11e2d3c75cb12f574fcaed074b2823b0aa331aba52440dc642689d79f470400067726f75705f3332314348ed39986e8d236adef6e7add19bec15c79b07fdf85c0a09982eb3c29cca7365726965735f33326866145571c2f9c2b8a3b52baf954f980c660a64ea1ed6ef16fd582ccf60d5e47fffffffffffffffc18b909bd6e69f35f770a46620bc09cbd5415e02a66e64189e2edf9bf91f683d220ea05824fabed0821744ba6fd6f41b6f9804e40f4d527d544619c60d103f860101ff15332967ea30f22d40820a20c40f234ee5c9b38e10047f967322a12d20a2ee9e000532626f785f6c6f636b5f3332495be2da0ca6024ddd25587dbd53f947a85f36ee2755bf4f378373d2f81ae57401f409317fffffffffffffff0fc364a34151d65147f5ad71e868d3bf0d5519b4e457353162c7406f8fd5256e",
      "DpVEovc5WvPqT8Ft6rbWUrn6fyyBXYUdHwZd9W39ajx4"
    ),
    TestVector(
      "0a470a2b0808104318d90222220a20ddd2a689964dd78b3266fd444cdb20becaefaae66ea855ceb9b8684494553a9612040a020a001a121a101a0a0a087fffffffffffffff3001380112340a28080510321a220a20cfff3ff7e96f8d3f95854479e1b644bd7d28cc62fde5f3858568f3b4e929435012080a060a040a0201f4123e0a28080510321a220a20213c18c16e2d64482b1ce8f61312c22e25a356426181f0ce131c1972269f520412121a101a0a0a087fffffffffffffff300138011a3c0a3a0a1408b11210ffffffffffffffff7f18efedfa81a23222220a20b6f764012ed2d6adf4e27d8743687fd4a73719cf7adb616634492b3215427b19",
      "0008430159696f5f7472616e73616374696f6e5f3332ddd2a689964dd78b3266fd444cdb20becaefaae66ea855ceb9b8684494553a9600ffff7fffffffffffffffffff0101ffff000532626f785f6c6f636b5f3332cfff3ff7e96f8d3f95854479e1b644bd7d28cc62fde5f3858568f3b4e929435001f4010532626f785f6c6f636b5f3332213c18c16e2d64482b1ce8f61312c22e25a356426181f0ce131c1972269f5204ffff7fffffffffffffffffff0101ffff09317fffffffffffffffb6f764012ed2d6adf4e27d8743687fd4a73719cf7adb616634492b3215427b19",
      "53u4M7FrbW8s7phxU15L8iYJTXRR8JNk1nzTWJiny4LK"
    ),
    TestVector(
      "0a470a2b0808104318d90222220a2094b015e783077db4b1c7f85646c42aac4a0aade8bdb352b5bea2f0f734323c9112040a020a001a121a101a0a0a087fffffffffffffff3001380112340a28080510321a220a20661312439eaab1c86b41697fe4ba08223b34a589b6e66fad0935c6bf364435c412080a060a040a0201f412640a28080510321a220a20f8abeb8501f3cdddecb4c3375a35b8df0afcda66807c3f2d7292170082f44c1312382a360a220a20bf25e5d205e9328244e95e1ad90bb5d22b9ed8794f07bbb5db0f00cbb229b2db120b0a0900fffffffffffffffe1a0308f4031a3c0a3a0a1408b11210ffffffffffffffff7f18eb808882a23222220a204f1c71c7562c9bf5468c1c1c0470a3f576a5a4a5e65167dc2911278716c7639f",
      "0008430159696f5f7472616e73616374696f6e5f333294b015e783077db4b1c7f85646c42aac4a0aade8bdb352b5bea2f0f734323c9100ffff7fffffffffffffffffff0101ffff000532626f785f6c6f636b5f3332661312439eaab1c86b41697fe4ba08223b34a589b6e66fad0935c6bf364435c401f4010532626f785f6c6f636b5f3332f8abeb8501f3cdddecb4c3375a35b8df0afcda66807c3f2d7292170082f44c137365726965735f3332bf25e5d205e9328244e95e1ad90bb5d22b9ed8794f07bbb5db0f00cbb229b2db00fffffffffffffffe01f4000009317fffffffffffffff4f1c71c7562c9bf5468c1c1c0470a3f576a5a4a5e65167dc2911278716c7639f",
      "23FUmvM8EhaEn2NxSDxGMQL3kYKy2sejgs1LdDWAjA9p"
    )
  )

  case class TestVector(txHex: String, txSignableHex: String, txId: String)
}
