package json.value.optics

import json.value.*
import monocle.Lens

import java.time.Instant
private[value] abstract class JsLenses[T<:Json[T]] {
  def value(path: JsPath): Lens[JsObj, JsValue] =
    val set = (value: JsValue) => (obj: JsObj) => obj.updated(path, value)
    Lens[JsObj, JsValue](_ (path))(set)

  def str(path: JsPath): Lens[JsObj, String] =
    val set = (s: String) => (obj: JsObj) => obj.updated(path, JsStr(s))
    Lens[JsObj, String](_.getStr(path).nn)(set)

  def int(path: JsPath): Lens[JsObj, Int] =
    val set = (s: Int) => (obj: JsObj) => obj.updated(path, JsInt(s))
    Lens[JsObj, Int](_.getInt(path).nn)(set)

  def long(path: JsPath): Lens[JsObj, Long] =
    val set = (s: Long) => (obj: JsObj) => obj.updated(path, JsLong(s))
    Lens[JsObj, Long](_.getLong(path).nn)(set)


  def number(path: JsPath): Lens[JsObj, BigDecimal] =
    val set = (s: BigDecimal) => (obj: JsObj) => obj.updated(path, JsBigDec(s))
    Lens[JsObj, BigDecimal](_.getNumber(path).nn)(set)

  def double(path: JsPath): Lens[JsObj, Double] =
    val set = (s: Double) => (obj: JsObj) => obj.updated(path, JsDouble(s))
    Lens[JsObj, Double](_.getDouble(path).nn)(set)

  def integral(path: JsPath): Lens[JsObj, BigInt] =
    val set = (s: BigInt) => (obj: JsObj) => obj.updated(path, JsBigInt(s))
    Lens[JsObj, BigInt](_.getIntegral(path).nn)(set)

  def bool(path: JsPath): Lens[JsObj, Boolean] =
    val set = (s: Boolean) => (obj: JsObj) => obj.updated(path, JsBool(s))
    Lens[JsObj, Boolean](_.getBool(path).nn)(set)

  def array(path: JsPath): Lens[JsObj, JsArray] =
    val set = (s: JsArray) => (obj: JsObj) => obj.updated(path, s)
    Lens[JsObj, JsArray](_.getArray(path).nn)(set)

  def obj(path: JsPath): Lens[JsObj, JsObj] =
    val set = (s: JsObj) => (obj: JsObj) => obj.updated(path, s)
    Lens[JsObj, JsObj](_.getObj(path).nn)(set)

  def instant(path: JsPath): Lens[JsObj, Instant] =
    val set = (s: Instant) => (obj: JsObj) => obj.updated(path, JsInstant(s))
    Lens[JsObj, Instant](_.getInstant(path).nn)(set)
}
