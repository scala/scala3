package json.value.gen

import json.value._
import org.scalacheck.Gen
import json.value.gen.*
import scala.collection.immutable

object JsArrayGen:
  def of(gen: Gen[JsValue]): Gen[JsArray] = 
    Gen.containerOf[immutable.Seq, JsValue](gen).map(v => JsArray(v))
  def noneEmptyOf(gen: Gen[JsValue]): Gen[JsArray] = 
    Gen.nonEmptyContainerOf[immutable.Seq, JsValue](gen).map(v => JsArray(v))
  def ofN(n: Int,
          gen: Gen[JsValue]
         ): Gen[JsArray] =
    if n == 0 then Gen.const(JsArray())
    else Gen.containerOfN[immutable.Seq, JsValue](n, gen).map(v => JsArray(v))
  
  def pairs(pairs: (JsPath, Gen[JsValue])*): Gen[JsArray] =
    if pairs.count(_._1.head match
      case Key(_) => true
      case Index(_) => false) > 0
    then throw UnsupportedOperationException("head of a path is a key")
    else genFromPairs[JsArray](Gen.const(JsArray.empty), pairs)