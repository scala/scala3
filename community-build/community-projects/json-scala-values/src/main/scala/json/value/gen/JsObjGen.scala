package json.value.gen

import json.value.gen.*
import json.value.*
import json.value.spec.JsObjSpec
import org.scalacheck.Gen

import scala.annotation.nowarn

object JsObjGen:
  def apply(pairs: (String, Gen[JsValue])*): Gen[JsObj] =
    @scala.annotation.tailrec
    def objGenRec(accGen: Gen[JsObj],
                  seq: Seq[(String, Gen[JsValue])]
                 ): Gen[JsObj] =
      if seq.isEmpty then accGen
      else
        val (headKey, headGen) = seq.head
        val gen = for
          acc <- accGen
          headVal <- headGen
        yield acc.updated(headKey,headVal)
        objGenRec(gen, seq.tail)

    objGenRec(Gen.const(JsObj.empty), pairs)
  def pairs(pairs: (JsPath, Gen[JsValue])*): Gen[JsObj] =
    if pairs.count(_._1.head match
      case Index(_) => true
      case _ => false) > 0
    then throw UnsupportedOperationException("head of a path is an index")
    else genFromPairs[JsObj](Gen.const(JsObj.empty), pairs)
