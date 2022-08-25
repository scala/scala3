package json.value.optics

import json.value.*
import monocle.Optional

import java.time.Instant

private[value] abstract class JsOptionals[T<:Json[T]] {

  def str(path: JsPath): Optional[JsObj, String] =
    val set = (s: String) => (obj: JsObj) => obj.updated(path, JsStr(s))
    Optional[JsObj, String](it=>Option(it.getStr(path)))(set)

  def int(path: JsPath): Optional[JsObj, Int] =
    val set = (s: Int) => (obj: JsObj) => obj.updated(path, JsInt(s))
    Optional[JsObj, Int](it=>it.getInt(path) match
      case null => None
      case x:Int => Some(x))(set)

  def long(path: JsPath): Optional[JsObj, Long] =
    val set = (s: Long) => (obj: JsObj) => obj.updated(path, JsLong(s))
    Optional[JsObj, Long](it=>it.getLong(path) match
      case null => None
      case x:Long => Some(x))(set)

  def number(path: JsPath): Optional[JsObj, BigDecimal] =
    val set = (s: BigDecimal) => (obj: JsObj) => obj.updated(path, JsBigDec(s))
    Optional[JsObj, BigDecimal](it=>Option(it.getNumber(path)))(set)

  def double(path: JsPath): Optional[JsObj, Double] =
    val set = (s: Double) => (obj: JsObj) => obj.updated(path, JsDouble(s))
    Optional[JsObj, Double](it=>it.getDouble(path) match
      case null => None
      case x:Double => Some(x))(set)

  def integral(path: JsPath): Optional[JsObj, BigInt] =
    val set = (s: BigInt) => (obj: JsObj) => obj.updated(path, JsBigInt(s))
    Optional[JsObj, BigInt](it=>Option(it.getIntegral(path)))(set)

  def bool(path: JsPath): Optional[JsObj, Boolean] =
    val set = (s: Boolean) => (obj: JsObj) => obj.updated(path, JsBool(s))
    Optional[JsObj, Boolean](it=>it.getBool(path) match
      case null => None
      case x:Boolean => Some(x))(set)

  def array(path: JsPath): Optional[JsObj, JsArray] =
    val set = (s: JsArray) => (obj: JsObj) => obj.updated(path, s)
    Optional[JsObj, JsArray](it=>Option(it.getArray(path)))(set)

  def obj(path: JsPath): Optional[JsObj, JsObj] =
    val set = (s: JsObj) => (obj: JsObj) => obj.updated(path, s)
    Optional[JsObj, JsObj](it=>Option(it.getObj(path)))(set)

  def instant(path: JsPath): Optional[JsObj, Instant] =
    val set = (s: Instant) => (obj: JsObj) => obj.updated(path, JsInstant(s))
    Optional[JsObj, Instant](it=>Option(it.getInstant(path)))(set)
}
