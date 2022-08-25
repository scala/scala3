package json.value.optics

import json.value.*
import monocle.Optional

import java.time.Instant

private[value] object JsArrayOptionals extends JsOptionals[JsArray] {
  
  def str(index: Int): Optional[JsArray, String] =
    val set = (s: String) => (arr: JsArray) => arr.updated(index, JsStr(s))
    Optional[JsArray, String](it=>Option(it.getStr(index)))(set)

  def int(index: Int): Optional[JsArray, Int] =
    val set = (s: Int) => (arr: JsArray) => arr.updated(index, JsInt(s))
    Optional[JsArray, Int](it=>it.getInt(index) match
      case null => None
      case x:Int => Some(x))(set)

  def long(index: Int): Optional[JsArray, Long] =
    val set = (s: Long) => (arr: JsArray) => arr.updated(index, JsLong(s))
    Optional[JsArray, Long](it=>it.getLong(index) match
      case null => None
      case x:Long => Some(x))(set)

  def number(index: Int): Optional[JsArray, BigDecimal] =
    val set = (s: BigDecimal) => (arr: JsArray) => arr.updated(index, JsBigDec(s))
    Optional[JsArray, BigDecimal](it=>Option(it.getNumber(index)))(set)

  def double(index: Int): Optional[JsArray, Double] =
    val set = (s: Double) => (arr: JsArray) => arr.updated(index, JsDouble(s))
    Optional[JsArray, Double](it=>it.getDouble(index) match
      case null => None
      case x:Double => Some(x))(set)

  def integral(index: Int): Optional[JsArray, BigInt] =
    val set = (s: BigInt) => (arr: JsArray) => arr.updated(index, JsBigInt(s))
    Optional[JsArray, BigInt](it=>Option(it.getIntegral(index)))(set)

  def bool(index: Int): Optional[JsArray, Boolean] =
    val set = (s: Boolean) => (arr: JsArray) => arr.updated(index, JsBool(s))
    Optional[JsArray, Boolean](it => it.getBool(index) match
      case null => None
      case x:Boolean => Some(x))(set)

  def array(index: Int): Optional[JsArray, JsArray] =
    val set = (s: JsArray) => (arr: JsArray) => arr.updated(index, s)
    Optional[JsArray, JsArray](it=>Option(it.getArray(index)))(set)

  def obj(index: Int): Optional[JsArray, JsObj] =
    val set = (s: JsObj) => (arr: JsArray) => arr.updated(index, s)
    Optional[JsArray, JsObj](it=>Option(it.getObj(index)))(set)

  def instant(index: Int): Optional[JsArray, Instant] =
    val set = (s: Instant) => (arr: JsArray) => arr.updated(index, JsInstant(s))
    Optional[JsArray, Instant](it=>Option(it.getInstant(index)))(set)
}
