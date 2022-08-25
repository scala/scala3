package json.value
import json.value.reduceHead
import json.value.{AbstractJsArray, AbstractJsObj}

import java.time.Instant
import scala.collection.immutable
import scala.collection.immutable.HashMap
/**
 * abstract class to reduce class file size in subclass.
 *
 * @param bindings the map of key and values
 */
private[json] abstract class AbstractJsObj(val bindings: immutable.Map[String, JsValue]) {

  /** Tests whether this json object contains a binding for a key.
   *
   * @param key the key
   * @return `true` if there is a binding for `key` in this map, `false` otherwise.
   */
  def contains(key: String): Boolean = bindings contains key


  /** Tests whether the Json object is empty.
   *
   * @return `true` if the Json object contains no elements, `false` otherwise.
   */
  def isEmpty: Boolean = bindings.isEmpty

  /** Selects the next element of the [[iterator]] of this Json object, throwing a
   * NoSuchElementException if the Json object is empty
   *
   * @return the next element of the [[iterator]] of this Json object.
   */
  def head: (String, JsValue) = bindings.head

  /** Optionally selects the next element of the [[iterator]] of this Json object.
   *
   * @return the first element of this Json object if it is nonempty.
   *         `None` if it is empty.
   */
  def headOption: Option[(String, JsValue)] = bindings.headOption

  /** Selects the last element of the iterator of this Json object, throwing a
   * NoSuchElementException if the Json object is empty
   *
   * @return the last element of the iterator of this Json object.
   */
  def last: (String, JsValue) = bindings.last

  /** Optionally selects the last element of the iterator of this Json object.
   *
   * @return the last element of the iterator of this Json object,
   *         `None` if it is empty.
   */
  def lastOption: Option[(String, JsValue)] = bindings.lastOption

  /** Collects all keys of this Json object in an iterable collection.
   *
   * @return the keys of this Json object as an iterable.
   */
  def keys: Iterable[String] = bindings.keys

  /** Retrieves the json.value which is associated with the given key. If there is no mapping
   * from the given key to a json.value, `JsNothing` is returned.
   *
   * @param key the key
   * @return the json.value associated with the given key
   */
  def apply(key: String): JsValue = bindings.applyOrElse(key, (_: String) => JsNothing)

  private[json] def apply(pos: Position): JsValue =
    pos.nn match
      case Key(name) => bindings.applyOrElse(name, (_: String) => JsNothing)
      case Index(_) => JsNothing


  /** The size of this Json object.
   * *
   *
   * @return the number of elements in this Json object.
   */
  def size: Int = bindings.size

  /** Collects all keys of this map in a set.
   *
   * @return a set containing all keys of this map.
   */
  def keySet: Set[String] = bindings.keySet


  def init: JsObj = JsObj(bindings.init)

  def tail: JsObj = JsObj(bindings.tail)

  /** Selects all elements of this Json object  which satisfy a predicate.
   *
   * @return a new Json object consisting of all elements of this Json object that satisfy the given predicate p. The order of the elements is preserved.
   */
  def filter(p: (JsPath, JsPrimitive) => Boolean): JsObj =
    JsObj(AbstractJsObj.filterByPair(p)(JsPath.root, bindings))

  def filter(p: JsPrimitive => Boolean): JsObj = JsObj(AbstractJsObj.filter(p)(bindings))

  def filterJsObj(p: (JsPath, JsObj) => Boolean): JsObj =
    JsObj(AbstractJsObj.filterObjByPair(p.nn)(JsPath.root, bindings))

  def filterJsObj(p: JsObj => Boolean): JsObj =
    JsObj(AbstractJsObj.filterObj(p)(bindings))

  def filterKeys(p: (JsPath, JsValue) => Boolean): JsObj =
    JsObj(AbstractJsObj.filterKeyByPair(p.nn)(JsPath.root, bindings))

  def filterKeys(p: String => Boolean): JsObj =
    JsObj(AbstractJsObj.filterKey(p)(bindings))

  def map(m: JsPrimitive => JsValue): JsObj = JsObj(AbstractJsObj.map(m.nn)(this.bindings))

  def map(m: (JsPath, JsPrimitive) => JsValue,
          p: (JsPath, JsPrimitive) => Boolean = (_, _) => true
         ): JsObj = JsObj(AbstractJsObj.mapByPair(m.nn, p.nn)(JsPath.root, this.bindings))


  def reduce[V](p: (JsPath, JsPrimitive) => Boolean = (_, _) => true,
                m: (JsPath, JsPrimitive) => V,
                r: (V, V) => V
               ): Option[V] =
    AbstractJsObj.reduceByPair(p,m,r)(JsPath.root, bindings, Option.empty)


  def mapKeys(m: (JsPath, JsValue) => String,
              p: (JsPath, JsValue) => Boolean = (_, _) => true
             ): JsObj =
    JsObj(AbstractJsObj.mapKeyByPair(m.nn, p.nn)(JsPath.root, bindings))


  def mapKeys(m: String => String): JsObj =
    JsObj(AbstractJsObj.mapKey(m.nn)(bindings, HashMap.empty))

  /** Returns an iterator of this Json object. Can be used only once
   *
   * @return an iterator
   */
  def iterator: Iterator[(String, JsValue)] = bindings.iterator

  /** Flatten this Json object into a `LazyList` of pairs of `(JsPath,JsValue)`
   * traversing recursively every noe-empty Json found along the way.
   *
   * @return a `LazyList` of pairs of `JsPath` and `JsValue`
   * */
  def flatten: LazyList[(JsPath, JsValue)] = AbstractJsObj.flatten(JsPath.root, bindings)


  def getArray(key: String, default: => JsArray): JsArray = apply(key) match
    case array: JsArray => array
    case _ => default

  def getInstant(key: String): Instant | Null =
    apply(key) match
      case JsInstant(i) => i
      case JsStr(s) => JsStr.instantPrism.getOption(s) match
        case Some(i) => i
        case None => null
      case _ => null

  def getInstant(key: String, default: => Instant): Instant =
    getInstant(key) match
      case i:Instant => i
      case null => default

  def getArray(key: String): JsArray | Null = apply(key) match
    case array: JsArray => array
    case _ => null

  def getObj(key: String, default: => JsObj): JsObj = apply(key) match
    case obj: JsObj => obj
    case _ => default

  def getObj(key: String): JsObj | Null = apply(key) match
    case obj: JsObj => obj
    case _ => null

  def getLong(key: String, default: => Long): Long = apply(key) match
    case JsLong(n) => n
    case JsInt(n) => n
    case _ => default

  def getLong(key: String): Long | Null = apply(key) match
    case JsLong(n) => n
    case JsInt(n) => n
    case _ => null

  def getInt(key: String, default: => Int): Int = apply(key) match
    case JsInt(n) => n
    case _ => default

  def getInt(key: String): Int | Null = apply(key) match
    case JsInt(n) => n
    case _ => null

  def getDouble(key: String, default: => Double): Double = apply(key) match
    case JsInt(n) => n
    case JsLong(n) => n.toDouble
    case JsDouble(n) => n
    case _ => default

  def getDouble(key: String): Double | Null = apply(key) match
    case JsInt(n) => n
    case JsLong(n) => n.toDouble
    case JsDouble(n) => n
    case _ => null

  def getNumber(key: String, default: => BigDecimal): BigDecimal = apply(key) match
    case JsInt(n) => BigDecimal(n)
    case JsLong(n) => BigDecimal(n)
    case JsDouble(n) => BigDecimal(n)
    case JsBigDec(n) => n
    case JsBigInt(n) => BigDecimal(n)
    case _ => default

  def getNumber(key: String): BigDecimal | Null = apply(key) match
    case JsInt(n) => BigDecimal(n)
    case JsLong(n) => BigDecimal(n)
    case JsDouble(n) => BigDecimal(n)
    case JsBigDec(n) => n
    case JsBigInt(n) => BigDecimal(n)
    case _ => null

  def getIntegral(key: String, default: => BigInt): BigInt = apply(key) match
    case JsInt(n) => BigInt(n)
    case JsLong(n) => BigInt(n)
    case JsBigInt(n) => n
    case _ => default

  def getIntegral(key: String): BigInt | Null = apply(key) match
    case JsInt(n) => BigInt(n)
    case JsLong(n) => BigInt(n)
    case JsBigInt(n) => n
    case _ => null

  def getBool(key: String, default: => Boolean): Boolean = apply(key) match
    case JsBool(n) => n
    case _ => default

  def getBool(key: String): Boolean | Null = apply(key) match
    case JsBool(n) => n
    case _ =>  null

  def getStr(key: String, default: => String): String = apply(key) match
    case JsStr(n) => n
    case _ => default

  def getStr(key: String): String | Null = apply(key) match
    case JsStr(n) => n
    case _ => null
}

object AbstractJsObj{
  def flatten(path: JsPath, map: Map[String, JsValue]): LazyList[(JsPath, JsValue)] =
    if map.isEmpty then return LazyList.empty
    val head = map.head
    head._2 match
      case JsObj(headMap) =>
        if headMap.isEmpty then (path / head._1, JsObj.empty) +: flatten(path, map.tail)
        else flatten(path / head._1, headMap) ++: flatten(path, map.tail)
      case JsArray(headSeq) =>
        if headSeq.isEmpty then (path / head._1, JsArray.empty) +: flatten(path, map.tail)
        else AbstractJsArray.flatten(path / head._1 / -1, headSeq) ++: flatten(path, map.tail)
      case _ => (path / head._1, head._2) +: flatten(path, map.tail)

  def map(m: JsPrimitive => JsValue)
         (input: Map[String, JsValue], result: Map[String, JsValue]=HashMap.empty): Map[String, JsValue] =
    def mapO = map(m)
    def mapA = AbstractJsArray.map(m)
    if input.isEmpty then result
    else
      val tail = input.tail
      input.head match
        case (key, JsObj(headMap)) => mapO(tail, result.updated(key, JsObj(mapO(headMap,Map.empty))))
        case (key, JsArray(headSeq)) => mapO(tail, result.updated(key, JsArray(mapA(headSeq,Seq.empty))))
        case (key, head: JsPrimitive) => mapO(tail, result.updated(key, m(head)))
        case (_,JsNothing) => assert(false,"JsNothing can't be inserted in a Json. It can only be returned.")

  def filterObjByPair(p: (JsPath, JsObj) => Boolean)
                     (path: JsPath, input: Map[String, JsValue], result: Map[String, JsValue] = HashMap.empty): Map[String, JsValue] =
    def filterO = filterObjByPair(p)
    def filterA = AbstractJsArray.filterObjByPair(p)
    if input.isEmpty then result
    else input.head match
      case (key, o: JsObj) =>
        if p(path / key, o) then filterO(path,
          input.tail,
          result.updated(key, JsObj(filterO(path / key, o.bindings,Map.empty))))
        else filterO(path, input.tail, result)
      case (key, JsArray(headSeq)) =>
        filterO(path,
          input.tail,
          result.updated(key, JsArray(filterA(path / key / -1, headSeq,Seq.empty))))
      case (key, head: JsValue) =>
        filterO(path, input.tail, result.updated(key, head))


  def filterObj(p: JsObj => Boolean)
               (input: Map[String, JsValue], result: Map[String, JsValue]= HashMap.empty): Map[String, JsValue] =
    def filterO = filterObj(p)
    def filterA = AbstractJsArray.filterObj(p)
    if input.isEmpty then result
    else input.head match
      case (key, o: JsObj) =>
        if p(o) then filterO(input.tail, result.updated(key, JsObj(filterO(o.bindings,Map.empty))))
        else filterO(input.tail, result)
      case (key, JsArray(headSeq)) =>
        filterO(input.tail, result.updated(key, JsArray(filterA(headSeq,Seq.empty))))
      case (key, head: JsValue) => filterO(input.tail, result.updated(key, head))


  def mapByPair(m: (JsPath, JsPrimitive) => JsValue,
                p: (JsPath, JsPrimitive) => Boolean)
               (path: JsPath, input: Map[String, JsValue],
                result: Map[String, JsValue]=HashMap.empty): Map[String, JsValue] =
    def mapO = mapByPair(m,p)
    def mapA = AbstractJsArray.mapByPair(m,p)
    if input.isEmpty then result
    else input.head match
      case (key, JsObj(headMap)) =>
        mapO(path, input.tail, result.updated(key, JsObj(mapO(path / key, headMap,Map.empty))))
      case (key, JsArray(headSeq)) =>
        mapO(path, input.tail, result.updated(key, JsArray(mapA(path / key / -1, headSeq,Seq.empty))))
      case (key, head: JsPrimitive) =>
        val headPath = path / key
        if p(headPath, head) then mapO(path, input.tail, result.updated(key, m(headPath, head)))
        else mapO(path, input.tail, result.updated(key, head))
      case (_,JsNothing) => assert(false,"JsNothing can't be inserted in a Json. It can only be returned.")



  def mapKeyByPair(m:(JsPath, JsValue) => String, p: (JsPath, JsValue) => Boolean)
                  (path: JsPath, input: Map[String, JsValue], result: Map[String, JsValue]=HashMap.empty): Map[String, JsValue] =
    def mapKO = mapKeyByPair(m,p)
    def mapKA = AbstractJsArray.mapKeyByPair(m,p)
    if input.isEmpty then result
    else input.head match
      case (key, o: JsObj) =>
        val headPath = path / key
        mapKO(path, input.tail,
          result.updated(if p(headPath, o) then m(headPath, o) else key,
            JsObj(mapKO(headPath, o.bindings,Map.empty))))
      case (key, arr: JsArray) =>
        val headPath = path / key
        mapKO(path, input.tail,
          result.updated(if p(headPath, arr) then m(headPath, arr) else key,
            JsArray(mapKA(path / key / -1, arr.seq,Seq.empty))))
      case (key, head: JsValue) =>
        val headPath = path / key
        mapKO(path,
          input.tail,
          result.updated(if p(headPath, head) then m(headPath, head) else key, head))



  def mapKey( m: String => String)
            (input: Map[String, JsValue], result: Map[String, JsValue]=HashMap.empty): Map[String, JsValue] =
    def mapKO = mapKey(m)
    def mapKA = AbstractJsArray.mapKey(m)

    if input.isEmpty then result
    else
      val tail = input.tail
      input.head match
        case (key, o: JsObj) =>
          mapKO(tail,
            result.updated(m(key), JsObj(mapKO(o.bindings,Map.empty))))
        case (key, arr: JsArray) =>
          mapKO(tail,
            result.updated(m(key), JsArray(mapKA(arr.seq,Seq.empty))))
        case (key, head: JsValue) =>
          mapKO(tail,
            result.updated(m(key), head))



  def filterKeyByPair(p: (JsPath, JsValue) => Boolean)
                     (path: JsPath, input: Map[String, JsValue],
                      result: Map[String, JsValue]=HashMap.empty): Map[String, JsValue] =
    def filterO = filterKeyByPair(p)
    def filterA = AbstractJsArray.filterKeyByPair(p)

    if input.isEmpty then result
    else
      val tail = input.tail
      input.head match
        case (key, o: JsObj) =>
          if p(path / key, o)
          then filterO(path,
            tail,
            result.updated(key, JsObj(filterO(path / key, o.bindings,Map.empty))))
          else filterO(path, tail, result)
        case (key, arr: JsArray) =>
          if p(path / key, arr)
          then filterO(path,
            tail,
            result.updated(key, JsArray(filterA(path / key / -1,
              arr.seq,
              Seq.empty))))
          else filterO(path, tail, result)
        case (key, head: JsValue) =>
          if p(path / key, head)
          then filterO(path, tail, result.updated(key, head))
          else filterO(path, tail, result)

  def filterKey(p: String => Boolean)
               (input: Map[String, JsValue],
                result: Map[String, JsValue]=HashMap.empty): Map[String, JsValue] =
    def filterO = filterKey(p)
    def filterA = AbstractJsArray.filterKey(p)
    if input.isEmpty then result
    else
      val tail = input.tail
      input.head match
        case (key, o: JsObj) =>
          if p(key) then filterO(tail, result.updated(key, JsObj(filterO(o.bindings,Map.empty))))
          else filterO(tail, result)
        case (key, arr: JsArray) =>
          if p(key) then filterO(tail, result.updated(key, JsArray(filterA(arr.seq,Seq.empty))))
          else filterO(tail, result)
        case (key, head: JsValue) =>
          if p(key) then filterO(tail, result.updated(key, head))
          else filterO(tail, result)

  def reduceByPair[V](p: (JsPath, JsPrimitive) => Boolean,
                      m: (JsPath, JsPrimitive) => V, r: (V, V) => V)
                     (path: JsPath, input: Map[String, JsValue],
                      acc: Option[V]=None): Option[V] =
    def reduceO = reduceByPair(p, m, r)
    def reduceA = AbstractJsArray.reduceByPair(p,m,r)
    if input.isEmpty then acc
    else
      val (key, head) = input.head
      val tail = input.tail
      head match
        case JsObj(headMap) =>
          reduceO(path, tail, reduceHead(r, acc, reduceO(path / key, headMap,None)))
        case JsArray(headSeq) =>
          reduceO(path, tail, reduceHead(r, acc, reduceA(path / key / -1, headSeq,None)))
        case value: JsPrimitive =>
          if p(path / key, value)
          then reduceO(path, tail, reduceHead(r, acc, m(path / key, value)))
          else reduceO(path, tail, acc)
        case JsNothing => assert(false,"JsNothing can't be inserted in a Json. It can only be returned.")




  def filter(p: JsPrimitive => Boolean)
            (input: Map[String, JsValue],
             result: Map[String, JsValue]=HashMap.empty): Map[String, JsValue] =
    def filterO = filter(p)
    def filterA = AbstractJsArray.filter(p)

    if input.isEmpty then result
    else
      val tail = input.tail
      input.head match
        case (key, JsObj(headMap)) =>
          filterO(tail, result.updated(key, JsObj(filterO(headMap,Map.empty))))
        case (key, JsArray(headSeq)) =>
          filterO(tail, result.updated(key, JsArray(filterA(headSeq,Seq.empty))))
        case (key, head: JsPrimitive) =>
          if p(head)
          then filterO(tail, result.updated(key, head))
          else filterO(tail, result)
        case (_,JsNothing) => assert(false,"JsNothing can't be inserted in a Json. It can only be returned.")

  def filterByPair(p: (JsPath, JsPrimitive) => Boolean)
                  (path: JsPath, input: Map[String, JsValue],
                   result: Map[String, JsValue]=HashMap.empty): Map[String, JsValue] =
    def filterO = filterByPair(p)
    def filterA = AbstractJsArray.filterByPair(p)
    if input.isEmpty then result
    else
      val tail = input.tail
      input.head match
        case (key, JsObj(headMap)) =>
          filterO(path,
            tail,
            result.updated(key, JsObj(filterO(path / key, headMap,Map.empty))))
        case (key, JsArray(headSeq)) =>
          filterO(path,
            tail,
            result.updated(key, JsArray(filterA(path / key / -1, headSeq,Seq.empty))))
        case (key, head: JsPrimitive) =>
          if p(path / key, head)
          then filterO(path, tail, result.updated(key, head))
          else filterO(path, tail, result)
        case (_JsNothing) => assert(false,"JsNothing can't be inserted in a Json. It can only be returned.")


}


