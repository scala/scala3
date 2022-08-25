package json.value

import json.value.*
import json.value.JsPath.MINUS_ONE

import java.time.Instant
import scala.collection.immutable.Seq
import scala.collection.immutable.HashMap

/**
 * abstract class to reduce class file size in subclass.
 *
 * @param seq the seq of values
 */
private[json] abstract class AbstractJsArray(private[json] val seq: Seq[JsValue]) {

  def isEmpty: Boolean = seq.isEmpty

  def length(): Int = seq.length

  def head: JsValue = seq.head

  def headOption: Option[JsValue] = seq.headOption

  def last: JsValue = seq.last

  def lastOption: Option[JsValue] = seq.lastOption

  def size: Int = seq.size

  def prependedAll(xs: IterableOnce[JsValue]): JsArray =
    JsArray(seq.prependedAll(xs.iterator.filterNot(e => e == JsNothing)))

  def appendedAll(xs: IterableOnce[JsValue]): JsArray =
    JsArray(seq.appendedAll(xs))

  def init: JsArray = JsArray(seq.init)

  def tail: JsArray = JsArray(seq.tail)

  def filter(p: (JsPath, JsPrimitive) => Boolean): JsArray =
    JsArray(AbstractJsArray.filterByPair(p)(MINUS_ONE, seq))

  def filterJsObj(p: (JsPath, JsObj) => Boolean): JsArray =
    JsArray(AbstractJsArray.filterObjByPair(p)(MINUS_ONE, seq))

  def filterKeys(p: (JsPath, JsValue) => Boolean): JsArray =
    JsArray(AbstractJsArray.filterKeyByPair(p)(MINUS_ONE, seq))

  def flatMap(f: JsValue => JsArray): JsArray = 
    JsArray(seq.flatMap(f))

  def iterator: Iterator[JsValue] = seq.iterator

  def map(m: (JsPath, JsPrimitive) => JsValue,
          p: (JsPath, JsPrimitive) => Boolean = (_, _) => true
         ): JsArray = 
    JsArray(AbstractJsArray.mapByPair(m,p)(MINUS_ONE, seq))

  def reduce[V](p: (JsPath, JsPrimitive) => Boolean = (_, _) => true,
                m: (JsPath, JsPrimitive) => V,
                r: (V, V) => V
               ): Option[V] = 
    AbstractJsArray.reduceByPair(p,m,r)(JsPath.root / MINUS_ONE, seq)

  def mapKeys(m: (JsPath, JsValue) => String,
              p: (JsPath, JsValue) => Boolean = (_, _) => true
             ): JsArray =
    JsArray(AbstractJsArray.mapKeyByPair(m,p)(MINUS_ONE, seq))

  def filter(p: JsPrimitive => Boolean): JsArray = 
    JsArray(AbstractJsArray.filter(p)(seq))

  def map(m: JsPrimitive => JsValue): JsArray = 
    JsArray(AbstractJsArray.map(m)(seq))


  def mapKeys(m: String => String): JsArray = 
    JsArray(AbstractJsArray.mapKey(m)(seq))


  def filterJsObj(p: JsObj => Boolean): JsArray = 
    JsArray(AbstractJsArray.filterObj(p)(seq))

  def filterKeys(p: String => Boolean): JsArray = JsArray(AbstractJsArray.filterKey(p)(seq))

  /**
   *
   * @return a lazy list of pairs of path and json.value
   */
  def flatten: LazyList[(JsPath, JsValue)] = 
    AbstractJsArray.flatten(MINUS_ONE, seq)

  private[json] def apply(pos: Position): JsValue =
    pos.nn match
      case Index(i) => apply(i)
      case Key(_) => json.value.JsNothing


  def apply(i: Int): JsValue =
    if i < 0 then JsNothing
    else seq.applyOrElse(i, (_: Int) => JsNothing)

  @scala.annotation.tailrec
  final private[json] def fillWith[E <: JsValue, P <: JsValue](seq: Seq[JsValue],
                                                               i: Int,
                                                               e: E,
                                                               p: P
                                                              ): Seq[JsValue] =
    if i< 0 then return seq
    if i < seq.length then seq.updated(i, e)
    else if i == seq.length
    then seq.appended(e)
    else fillWith(seq.appended(p), i, e, p)



  def getArray(index: Int, 
               default: => JsArray): JsArray = apply(index) match
    case array: JsArray => array
    case _ => default

  def getArray(index: Int): JsArray | Null = apply(index) match
    case array: JsArray => array
    case _ => null

  def getInstant(index: Int): Instant | Null = apply(index) match
    case JsInstant(i) => i
    case JsStr(s)  => JsStr.instantPrism.getOption(s) match
      case Some(i) => i
      case None => null
    case _ => null

  def getInstant(index: Int, 
                 default: => Instant): Instant =
    getInstant(index) match
      case i: Instant => i
      case null => default

  def getObj(index: Int, 
             default: => JsObj): JsObj = apply(index) match
    case obj: JsObj => obj
    case _ => default

  def getObj(index: Int): JsObj | Null = apply(index) match
    case obj: JsObj => obj
    case _ => null

  def getLong(index: Int, 
              default: => Long): Long = apply(index) match
    case JsLong(n) => n
    case JsInt(n) => n
    case _ => default

  def getLong(index: Int): Long | Null = apply(index) match
    case JsLong(n) => n
    case JsInt(n) => n
    case _ => null

  def getInt(index: Int, 
             default: => Int): Int = apply(index) match
    case JsInt(n) => n
    case _ => default

  def getInt(index: Int): Int | Null = apply(index) match
    case JsInt(n) => n
    case _ => null

  def getDouble(index: Int, 
                default: => Double): Double = apply(index) match
    case JsInt(n) => n
    case JsLong(n) => n.toDouble
    case JsDouble(n) => n
    case _ => default

  def getDouble(index: Int): Double | Null = apply(index) match
    case JsInt(n) => n
    case JsLong(n) => n.toDouble
    case JsDouble(n) => n
    case _ => null

  def getNumber(index: Int, default: => BigDecimal): BigDecimal = apply(index) match
    case JsInt(n) => BigDecimal(n)
    case JsLong(n) => BigDecimal(n)
    case JsDouble(n) => BigDecimal(n)
    case JsBigDec(n) => n
    case JsBigInt(n) => BigDecimal(n)
    case _ => default

  def getNumber(index: Int): BigDecimal | Null = apply(index) match
    case JsInt(n) => BigDecimal(n)
    case JsLong(n) => BigDecimal(n)
    case JsDouble(n) => BigDecimal(n)
    case JsBigDec(n) => n
    case JsBigInt(n) => BigDecimal(n)
    case _ => null

  def getIntegral(index: Int, default: => BigInt): BigInt = apply(index) match
    case JsInt(n) => BigInt(n)
    case JsLong(n) => BigInt(n)
    case JsBigInt(n) => n
    case _ => default

  def getIntegral(index: Int): BigInt | Null = apply(index) match
    case JsInt(n) => BigInt(n)
    case JsLong(n) => BigInt(n)
    case JsBigInt(n) => n
    case _ => null

  def getBool(index: Int, default: => Boolean): Boolean = apply(index) match
    case JsBool(n) => n
    case _ => default

  def getBool(index: Int): Boolean | Null = apply(index) match
    case JsBool(n) => n
    case _ =>  null

  def getStr(index: Int, default: => String): String = apply(index) match
    case JsStr(n) => n
    case _ => default

  def getStr(index: Int): String | Null = apply(index) match
    case JsStr(n) => n
    case _ => null
}

object AbstractJsArray {


  def flatten(path: JsPath, seq: Seq[JsValue]): LazyList[(JsPath, JsValue)] =
    if seq.isEmpty then return LazyList.empty
    val head: JsValue = seq.head
    val headPath: JsPath = path.inc
    head match
      case JsArray(headSeq) =>
        if headSeq.isEmpty then (headPath, JsArray.empty) +: flatten(headPath, seq.tail)
        else flatten(headPath / MINUS_ONE, headSeq) ++: flatten(headPath, seq.tail)
      case JsObj(headMap) =>
        if headMap.isEmpty then (headPath, JsObj.empty) +: flatten(headPath, seq.tail)
        else AbstractJsObj.flatten(headPath, headMap) ++: flatten(headPath, seq.tail)
      case _ => (headPath, head) +: flatten(headPath, seq.tail)

  def filterKey( p: String => Boolean)
               (input: Seq[JsValue], 
                result: Seq[JsValue]=Seq.empty): Seq[JsValue] =
    def filterKA = filterKey(p)
    def filterKO = AbstractJsObj.filterKey(p)

    if input.isEmpty then result
    else
      val tail = input.tail
      input.head match
        case JsObj(headMap) =>
          filterKA(tail, result.appended(JsObj(filterKO(headMap,Map.empty))))
        case JsArray(headSeq) =>
          filterKA(tail, result.appended(JsArray(filterKA(headSeq,Seq.empty))))
        case head: JsValue =>
          filterKA(tail, result.appended(head))


  def remove(i: Int, seq: Seq[JsValue]): Seq[JsValue] =
    if seq.isEmpty then seq
    else if i >= seq.size || i < 0 then seq
    else if i == 0 then seq.tail
    else
      val (prefix, suffix): (Seq[JsValue], Seq[JsValue]) = seq.splitAt(i)
      prefix.appendedAll(suffix.tail)


  def reduceByPair[V](p: (JsPath, JsPrimitive) => Boolean, 
                      m: (JsPath, JsPrimitive) => V, r: (V, V) => V)
                     (path: JsPath, input: Seq[JsValue], 
                      acc: Option[V] = None): Option[V] =
    def reduceObj = AbstractJsObj.reduceByPair(p,m,r)
    def reduceArr = reduceByPair(p,m,r)
    if input.isEmpty then acc
    else
      val headPath = path.inc
      val head = input.head
      val tail = input.tail
      head match
        case JsObj(headMap) =>
          reduceArr(headPath, tail, reduceHead(r, acc, reduceObj(headPath, headMap,None)))
        case JsArray(headSeq) =>
          val a: Option[V] = reduceArr(headPath / MINUS_ONE, headSeq, None)
          reduceArr(headPath, tail, reduceHead(r, acc, a))
        case value: JsPrimitive =>
          if p(headPath, value)
          then reduceArr(headPath, tail, reduceHead(r, acc, m(headPath, value)))
          else reduceArr(headPath, tail, acc)
        case JsNothing => assert(false,"JsNothing can't be inserted in a Json. It can only be returned.")


  def filterObjByPair(p: (JsPath, JsObj) => Boolean)
                     (path: JsPath, 
                      input: Seq[JsValue], 
                      result: Seq[JsValue]=Seq.empty): Seq[JsValue] =
    def filterA = filterObjByPair(p)
    def filterO = AbstractJsObj.filterObjByPair(p)
    if (input.isEmpty) result
    else
      val headPath: JsPath = path.inc
      val tail = input.tail
      input.head match
        case o: JsObj =>
          if p(headPath, o)
          then filterA(headPath, tail, result.appended(JsObj(filterO(headPath, o.bindings,Map.empty))))
          else filterA(headPath, tail, result)
        case JsArray(headSeq) =>
          filterA(headPath, tail, result.appended(JsArray(filterA(headPath / MINUS_ONE, headSeq,Seq.empty))))
        case head: JsValue => filterA(headPath, tail, result.appended(head))


  def filterObj(p: JsObj => Boolean)
               (input: Seq[JsValue], 
                result: Seq[JsValue]=Seq.empty): Seq[JsValue] =
    def filterA = filterObj(p)
    def filterO = AbstractJsObj.filterObj(p)
    if (input.isEmpty) result
    else
      input.head match
        case o: JsObj =>
          if p(o)
          then filterA(input.tail, result.appended(JsObj(filterO(o.bindings,Map.empty))))
          else filterA(input.tail, result)
        case JsArray(headSeq) =>
          filterA(input.tail, result.appended(JsArray(filterA(headSeq,Seq.empty))))
        case head: JsValue =>
          filterA(input.tail, result.appended(head))



  def filterByPair(p: (JsPath, JsPrimitive) => Boolean)
                  (path: JsPath, 
                   input: Seq[JsValue], 
                   result: Seq[JsValue] = Seq.empty): Seq[JsValue] =
    def filterA = filterByPair(p)
    def filterO = AbstractJsObj.filterByPair(p)
    if input.isEmpty then result
    else
      val headPath: JsPath = path.inc
      input.head match
        case JsObj(headMap) =>
          filterA(headPath, input.tail, result.appended(JsObj(filterO(headPath, headMap,Map.empty))))
        case JsArray(headSeq) =>
          filterA(headPath, input.tail, result.appended(JsArray(filterA(headPath / MINUS_ONE, headSeq,Seq.empty))))
        case head: JsPrimitive =>
          if p(headPath, head)
          then filterA(headPath, input.tail, result.appended(head))
          else filterA(headPath, input.tail, result)
        case JsNothing => assert(false,"JsNothing can't be inserted in a Json. It can only be returned.")



  def filter(p: JsPrimitive => Boolean)
            (input: Seq[JsValue], 
             result: Seq[JsValue]=Seq.empty): Seq[JsValue] =
    def filterA = filter(p)
    def filterO = AbstractJsObj.filter(p)
    if input.isEmpty then result
    else input.head match
      case JsObj(headMap) =>
        filterA(input.tail, result.appended(JsObj(filterO(headMap,Map.empty))))
      case JsArray(headSeq) =>
        filterA(input.tail, result.appended(JsArray(filterA(headSeq,Seq.empty))))
      case head: JsPrimitive =>
        if p(head)
        then filterA(input.tail, result.appended(head))
        else filterA(input.tail, result)
      case JsNothing => assert(false,"JsNothing can't be inserted in a Json. It can only be returned.")



  def mapByPair(m: (JsPath, JsPrimitive) => JsValue, p: (JsPath, JsPrimitive) => Boolean)
               (path: JsPath, 
                input: Seq[JsValue], 
                result: Seq[JsValue]=Seq.empty): Seq[JsValue] =
    def mapA = mapByPair(m,p)
    def mapO = AbstractJsObj.mapByPair(m,p)
    if input.isEmpty then result
    else
      val headPath: JsPath = path.inc
      val tail = input.tail
      input.head match
        case JsObj(headMap) =>
          mapA(headPath, tail, result.appended(JsObj(mapO(headPath, headMap,Map.empty))))
        case JsArray(headSeq) =>
          mapA(headPath, tail, result.appended(JsArray(mapA(headPath / MINUS_ONE, headSeq,Seq.empty))))
        case head: JsPrimitive =>
          if p(headPath, head)
          then mapA(headPath, tail, result.appended(m(headPath, head)))
          else mapA(headPath, tail, result.appended(head))
        case JsNothing => assert(false,"JsNothing can't be inserted in a Json. It can only be returned.")


  def map(m: JsPrimitive => JsValue)
         (input: Seq[JsValue], result: Seq[JsValue]=Seq.empty): Seq[JsValue] =
    def mapA = map(m)
    def mapO = AbstractJsObj.map(m)
    if input.isEmpty then result
    else
      val tail = input.tail
      input.head match
        case JsObj(headMap) =>
          mapA(tail, result.appended(JsObj(mapO(headMap,Map.empty))))
        case JsArray(headSeq) =>
          mapA(tail, result.appended(JsArray(mapA(headSeq,Seq.empty))))
        case head: JsPrimitive =>
          mapA(tail, result.appended(m(head)))
        case JsNothing => assert(false,"JsNothing can't be inserted in a Json. It can only be returned.")


  def mapKeyByPair(m: (JsPath, JsValue) => String, p: (JsPath, JsValue) => Boolean)
                  (path: JsPath, 
                   input: Seq[JsValue], 
                   result: Seq[JsValue]=Vector.empty): Seq[JsValue] =
    def mapKA = mapKeyByPair(m,p)
    def mapKO = AbstractJsObj.mapKeyByPair(m,p)
    if input.isEmpty then result
    else
      val headPath = path.inc
      val tail = input.tail
      input.head match
        case JsObj(headMap) =>
          mapKA(headPath, tail, result.appended(JsObj(mapKO(headPath, headMap,Map.empty))))
        case JsArray(headSeq) =>
          mapKA(headPath, tail, result.appended(JsArray(mapKA(headPath / MINUS_ONE, headSeq,Seq.empty))))
        case head: JsValue =>
          mapKA(headPath, tail, result.appended(head))


  def mapKey(m: String => String)
            (input: Seq[JsValue], 
             result: Seq[JsValue]=Seq.empty): Seq[JsValue] =
    def mapKA = mapKey(m)
    def mapKO = AbstractJsObj.mapKey(m)

    if input.isEmpty then result
    else
      val tail = input.tail
      input.head match
        case JsObj(headMap) =>
          mapKA(tail, result.appended(JsObj(mapKO(headMap,Map.empty))))
        case JsArray(headSeq) =>
          mapKA(tail, result.appended(JsArray(mapKA(headSeq,Seq.empty))))
        case head: JsValue =>
          mapKA(tail, result.appended(head))

  def filterKeyByPair(p: (JsPath, JsValue) => Boolean)
                     (path: JsPath, 
                      input: Seq[JsValue], 
                      result: Seq[JsValue]=Vector.empty): Seq[JsValue] =
    def filterKA = filterKeyByPair(p)
    def filterKO = AbstractJsObj.filterKeyByPair(p)
    if input.isEmpty then result
    else
      val headPath = path.inc
      val tail = input.tail
      input.head match
        case JsObj(headMap) =>
          filterKA(headPath, tail, result.appended(JsObj(filterKO(headPath, headMap,Map.empty))))
        case JsArray(headSeq) =>
          filterKA(headPath, tail, result.appended(JsArray(filterKA(headPath / MINUS_ONE, headSeq,Seq.empty))))
        case head: JsValue =>
          filterKA(headPath, tail, result.appended(head))
  
}


