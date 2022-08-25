package json.value.gen

import json.value.{JsArray, JsValue}
import org.scalacheck.Gen

object TupleGen:

  def apply(seq: Gen[JsValue]*): Gen[JsArray] =
    @scala.annotation.tailrec
    def arrGenRec(acc: Gen[JsArray], gens: collection.Seq[Gen[JsValue]]): Gen[JsArray] =
      if gens.isEmpty then acc
      else
        val gen =
          for result <- acc
              headVal <- gens.head
          yield result.appended(headVal)
        arrGenRec(gen, gens.tail)
    arrGenRec(Gen.const(JsArray.empty), seq)
