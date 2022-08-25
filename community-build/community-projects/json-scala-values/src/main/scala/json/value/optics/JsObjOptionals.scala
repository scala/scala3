package json.value.optics

import json.value.*
import monocle.Optional

import java.time.Instant

private[value] object JsObjOptionals extends JsOptionals[JsObj] {


  def str(key: String): Optional[JsObj, String] =
    val set = (s: String) => (obj: JsObj) => obj.updated(key, JsStr(s))
    Optional[JsObj, String](it=>Option(it.getStr(key)))(set)

  def int(key: String): Optional[JsObj, Int] =
    val set = (s: Int) => (obj: JsObj) => obj.updated(key, JsInt(s))
    Optional[JsObj, Int](it=>it.getInt(key) match
      case null => None
      case x:Int => Some(x))(set)

  def long(key: String): Optional[JsObj, Long] =
    val set = (s: Long) => (obj: JsObj) => obj.updated(key, JsLong(s))
    Optional[JsObj, Long](it=>it.getLong(key) match
      case null => None
      case x:Long => Some(x))(set)

  def number(key: String): Optional[JsObj, BigDecimal] =
    val set = (s: BigDecimal) => (obj: JsObj) => obj.updated(key, JsBigDec(s))
    Optional[JsObj, BigDecimal](it=>Option(it.getNumber(key)))(set)

  def double(key: String): Optional[JsObj, Double] =
    val set = (s: Double) => (obj: JsObj) => obj.updated(key, JsDouble(s))
    Optional[JsObj, Double](it=>it.getDouble(key) match
      case null => None
      case x:Double => Some(x))(set)

  def integral(key: String): Optional[JsObj, BigInt] =
    val set = (s: BigInt) => (obj: JsObj) => obj.updated(key, JsBigInt(s))
    Optional[JsObj, BigInt](it=>Option(it.getIntegral(key)))(set)

  def bool(key: String): Optional[JsObj, Boolean] =
    val set = (s: Boolean) => (obj: JsObj) => obj.updated(key, JsBool(s))
    Optional[JsObj, Boolean](it=>it.getBool(key) match
      case null => None
      case x:Boolean => Some(x))(set)

  def array(key: String): Optional[JsObj, JsArray] =
    val set = (s: JsArray) => (obj: JsObj) => obj.updated(key, s)
    Optional[JsObj, JsArray](it=>Option(it.getArray(key)))(set)

  def obj(key: String): Optional[JsObj, JsObj] =
    val set = (s: JsObj) => (obj: JsObj) => obj.updated(key, s)
    Optional[JsObj, JsObj](it=>Option(it.getObj(key)))(set)

  def instant(key: String): Optional[JsObj, Instant] =
    val set = (s: Instant) => (obj: JsObj) => obj.updated(key, JsInstant(s))
    Optional[JsObj, Instant](it=>Option(it.getInstant(key)))(set)
}
