package json.value.optics

import json.value.*
import monocle.Lens

import java.time.Instant

private[value] object JsObjLenses extends JsLenses[JsObj] :

  def value(key: String): Lens[JsObj, JsValue] = 
    val set = (value: JsValue) => (obj: JsObj) => obj.updated(key, value)
    Lens[JsObj, JsValue](_ (key))(set)

  def str(key: String): Lens[JsObj, String] = 
    val set = (s: String) => (obj: JsObj) => obj.updated(key, JsStr(s))
    Lens[JsObj, String](_.getStr(key).nn)(set)

  def int(key: String): Lens[JsObj, Int] = 
    val set = (s: Int) => (obj: JsObj) => obj.updated(key, JsInt(s))
    Lens[JsObj, Int](_.getInt(key).nn)(set)

  def long(key: String): Lens[JsObj, Long] = 
    val set = (s: Long) => (obj: JsObj) => obj.updated(key, JsLong(s))
    Lens[JsObj, Long](_.getLong(key).nn)(set)


  def number(key: String): Lens[JsObj, BigDecimal] = 
    val set = (s: BigDecimal) => (obj: JsObj) => obj.updated(key, JsBigDec(s))
    Lens[JsObj, BigDecimal](_.getNumber(key).nn)(set)


  def double(key: String): Lens[JsObj, Double] = 
    val set = (s: Double) => (obj: JsObj) => obj.updated(key, JsDouble(s))
    Lens[JsObj, Double](_.getDouble(key).nn)(set)


  def integral(key: String): Lens[JsObj, BigInt] = 
    val set = (s: BigInt) => (obj: JsObj) => obj.updated(key, JsBigInt(s))
    Lens[JsObj, BigInt](_.getIntegral(key).nn)(set)

  def bool(key: String): Lens[JsObj, Boolean] = 
    val set = (s: Boolean) => (obj: JsObj) => obj.updated(key, JsBool(s))
    Lens[JsObj, Boolean](_.getBool(key).nn)(set)

  def obj(key: String): Lens[JsObj, JsObj] = 
    val set = (s: JsObj) => (obj: JsObj) => obj.updated(key, s)
    Lens[JsObj, JsObj](_.getObj(key).nn)(set)

  def array(key: String): Lens[JsObj, JsArray] = 
    val set = (s: JsArray) => (obj: JsObj) => obj.updated(key, s)
    Lens[JsObj, JsArray](_.getArray(key).nn)(set)

  def instant(key: String): Lens[JsObj, Instant] = 
    val set = (s: Instant) => (obj: JsObj) => obj.updated(key, JsInstant(s))
    Lens[JsObj, Instant](_.getInstant(key).nn)(set)