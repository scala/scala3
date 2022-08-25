package json.value.optics

import json.value.*
import monocle.Lens

import java.time.Instant

private[value] object JsArrayLenses extends JsLenses[JsArray] :

  def value(index: Int): Lens[JsArray, JsValue] =
    val set = (value: JsValue) => (obj: JsArray) => obj.updated(index, value)
    Lens[JsArray, JsValue](_ (index))(set)

  def str(index: Int): Lens[JsArray, String] =
    val set = (s: String) => (obj: JsArray) => obj.updated(index, JsStr(s))
    Lens[JsArray, String](_.getStr(index).nn)(set)

  def int(index: Int): Lens[JsArray, Int] =
    val set = (s: Int) => (obj: JsArray) => obj.updated(index, JsInt(s))
    Lens[JsArray, Int](_.getInt(index).nn)(set)

  def long(index: Int): Lens[JsArray, Long] =
    val set = (s: Long) => (obj: JsArray) => obj.updated(index, JsLong(s))
    Lens[JsArray, Long](_.getLong(index).nn)(set)

  def number(index: Int): Lens[JsArray, BigDecimal] =
    val set = (s: BigDecimal) => (obj: JsArray) => obj.updated(index, JsBigDec(s))
    Lens[JsArray, BigDecimal](_.getNumber(index).nn)(set)


  def double(index: Int): Lens[JsArray, Double] =
    val set = (s: Double) => (obj: JsArray) => obj.updated(index, JsDouble(s))
    Lens[JsArray, Double](_.getDouble(index).nn)(set)

  def integral(index: Int): Lens[JsArray, BigInt] =
    val set = (s: BigInt) => (obj: JsArray) => obj.updated(index, JsBigInt(s))
    Lens[JsArray, BigInt](_.getIntegral(index).nn)(set)


  def bool(index: Int): Lens[JsArray, Boolean] =
    val set = (s: Boolean) => (obj: JsArray) => obj.updated(index, JsBool(s))
    Lens[JsArray, Boolean](_.getBool(index).nn)(set)

  def obj(index: Int): Lens[JsArray, JsObj] =
    val set = (s: JsObj) => (obj: JsArray) => obj.updated(index, s)
    Lens[JsArray, JsObj](_.getObj(index).nn)(set)

  def array(index: Int): Lens[JsArray, JsArray] =
    val set = (s: JsArray) => (obj: JsArray) => obj.updated(index, s)
    Lens[JsArray, JsArray](_.getArray(index).nn)(set)

  def instant(index: Int): Lens[JsArray, Instant] =
    val set = (s: Instant) => (obj: JsArray) => obj.updated(index, JsInstant(s))
    Lens[JsArray, Instant](_.getInstant(index).nn)(set)
