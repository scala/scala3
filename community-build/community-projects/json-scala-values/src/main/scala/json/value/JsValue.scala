package json.value
import json.value.spec.codec.{JsArrayCodec, JsObjCodec}
import json.value.spec.parser.{DecimalConf, JsArrayOfParser, JsObjParser, JsValueParser}
import com.github.plokhotnyuk.jsoniter_scala.core.{ReaderConfig, WriterConfig, readFromArray, readFromString, writeToArray, writeToStream, writeToString}
import json.value.Json.prism
import json.value.optics.{JsArrayLenses, JsObjLenses, JsObjOptionals,JsArrayOptionals}
import org.scalacheck.Gen
import json.value.spec.parser.ParserConf
import monocle.Prism

import java.io.{ByteArrayOutputStream, OutputStream}
import java.math.MathContext
import java.util.{Objects, Optional}
import scala.collection.immutable
import scala.collection.immutable.HashMap
import scala.util.{Success, Try}
import monocle.Lens

import java.time.Instant
import java.time.format.DateTimeParseException


/** Represents any element in a Json.
 * All the json.value types are immutable, being the Json array and Json object implemented with
 * persistent data structures
 */
sealed trait JsValue:
  def isNull:Boolean = this == JsNull
  def noneNull:Boolean = !isNull
  /**
   * Every implementation of this trait has an unique identifier in order.
   *
   * @return unique identifier of the type
   */


/** Represents any value in a Json that is not a container, i.e. a Json object or a Json array
 *
 */
sealed trait JsPrimitive extends JsValue

final case class JsInstant(value:Instant) extends JsPrimitive:

  override def toString: String = value.toString
  override def equals(o: Any): Boolean =
    if o == null then false
    else
      o match
        case v:JsValue => JsInstant.prism.getOption(v).contains(value)
        case _ => false

  override def hashCode(): Int = Objects.hashCode(value.toString)
object JsInstant:
  val prism: Prism[JsValue, Instant] = Prism((value: JsValue) => value match {
    case JsInstant(s) => Some(s)
    case JsStr(s) =>
      try Some(Instant.parse(s))
      catch _ => None
    case _ => None
  })(JsInstant(_))





/** Represents an immutable string
 *
 * @param value the value of the string
 */
final case class JsStr(value: String) extends JsPrimitive:

  override def toString: String = value
  override def equals(o: Any): Boolean =
    if o == null then false
    else
      o match
        case JsInstant(s) => JsStr.instantPrism.reverseGet(s).contains(value)
        case JsStr(s) =>  s == value
        case _ => false

  override def hashCode(): Int = Objects.hashCode(value)


object JsStr:
  val prism: Prism[JsValue, String] = Prism((value: JsValue) => value match {
    case JsStr(s) => Some(s)
    case _ => None
  })(JsStr(_))

  val instantPrism: Prism[String, Instant] =
    Prism((s: String) =>
      try Some(Instant.parse(s))
      catch _ => None
    )(_.toString)

/** Represents an immutable number
 *
 */
sealed trait JsNumber extends JsPrimitive

/**
 * Represents an immutable number of type `Int`
 *
 * @param value the value of the number
 */
final case class JsInt(value: Int) extends JsNumber {

  override def toString: String = value.toString

  override def equals(that: Any): Boolean =
    if that == null then false
    else that match
      case JsInt(n) => value == n
      case JsLong(n) => value.toLong == n
      case JsBigInt(n) => Try(n.bigInteger.intValueExact) match
        case Success(m) => value == m
        case _ => false
      case JsDouble(n) => value.toDouble == n
      case JsBigDec(n) => Try(n.toIntExact) match
        case Success(m) => value == m
        case _ => false
      case _ => false

  override def hashCode(): Int = value


}

object JsInt:

  val prism: Prism[JsValue, Int] = Prism((value: JsValue) => value match {
    case JsInt(value) => Some(value)
    case _ => None
  })(JsInt(_))

/**
 * Represents an immutable number of type `Double`
 *
 * @param value the value of the number
 */
final case class JsDouble(value: Double) extends JsNumber {

  override def toString: String = value.toString

  /** returns true if that represents the same number, no matter the type it's wrapped in:
   *
   * JsInt(1)    ==    JsDouble(1.0)   // true
   * JsLong(1)   ==    JsDouble(1.0)   // true
   * JsBigInt(1) ==    JsDouble(1.0)   // true
   *
   */
  override def equals(that: Any): Boolean = {
    if that == null then false
    else that match
      case JsInt(n) => value == n.toDouble
      case JsLong(n) => value == n.toDouble
      case JsBigInt(n) => BigDecimal(value).toBigIntExact match
        case Some(m) => n == m
        case _ => false
      case JsDouble(n) => value == n
      case JsBigDec(n) =>
        BigDecimal(value).compare(n) == 0
      case _ => false
  }


  override def hashCode(): Int =
    val decimal = BigDecimal(value)
    Try(decimal.toIntExact) match
      case Success(n) => n
      case _ => Try(decimal.toLongExact) match
        case Success(n) =>  
          (n ^ (n >>> 32)).toInt
        case _ => decimal.toBigIntExact match
          case Some(n) => n.hashCode
          case _ => decimal.hashCode


}

object JsDouble:
  val prism: Prism[JsValue, Double] =
    Prism((value: JsValue) => value match
      case JsLong(value) => Some(value.toDouble)
      case JsInt(value) => Some(value.toDouble)
      case JsDouble(value) => Some(value)
      case _ => None
    )((d: Double) => JsDouble(d))


/**
 * Represents an immutable number of type `Long`
 *
 * @param value the value of the number
 */
final case class JsLong(value: Long) extends JsNumber {


  override def toString: String = value.toString


  override def equals(that: Any): Boolean =
    if that == null then false
    else that match
      case JsInt(n) => value == n.toLong
      case JsLong(n) => value == n
      case JsBigInt(n) => Try(n.bigInteger.longValueExact()) match
        case Success(m) => value == m
        case _ => false
      case JsDouble(n) => value.toDouble == n
      case JsBigDec(n) => Try(n.toLongExact) match
        case Success(m) => value == m
        case _ => false
      case _ => false


  override def hashCode(): Int = Try(Math.toIntExact(value)) match
    case Success(n) => n
    case _ => (value ^ (value >>> 32)).toInt


}

object JsLong:
  val prism: Prism[JsValue, Long] =
    Prism((value: JsValue) => value match
      case JsLong(value) => Some(value)
      case JsInt(value) => Some(value.toLong)
      case _ => None)(JsLong(_))

/**
 * Represents an immutable number of type `BigDecimal`
 *
 * @param value the value of the number
 */
final case class JsBigDec(value: BigDecimal) extends JsNumber {
  override def equals(that: Any): Boolean =
    if that == null then false
    else that match
      case JsInt(n) => Try(value.toIntExact) match
        case Success(m) => n == m
        case _ => false
      case JsLong(n) => Try(value.toLongExact) match
        case Success(m) => n == m
        case _ => false
      case JsBigInt(n) => value.toBigIntExact match
        case Some(m) => n == m
        case _ => false
      case JsDouble(n) => BigDecimal(n).compare(value) == 0
      case JsBigDec(n) => value.compareTo(n) == 0
      case _ => false

  override def hashCode(): Int =
    Try(value.toIntExact) match
      case Success(n) => n
      case _ => Try(value.toLongExact) match
        case Success(n) => (n ^ (n >>> 32)).toInt
        case _ => value.toBigIntExact match
          case Some(n) => n.hashCode()
          case _ => value.hashCode


}


/**
 * Represents an immutable number of type `BigInt`
 *
 * @param value the value of the number
 */
final case class JsBigInt(value: BigInt) extends JsNumber {
  override def toString: String = value.toString

  override def equals(that: Any): Boolean = {
    if (that == null) false
    else that match {
      case JsInt(n) => Try(value.bigInteger.intValueExact) match {
        case Success(m) => n == m
        case _ => false
      }
      case JsLong(n) => value == n
      case JsBigInt(n) => value == n
      case JsDouble(n) => BigDecimal(n).toBigIntExact match {
        case Some(m) => value == m
        case _ => false
      }
      case JsBigDec(n) => n.toBigIntExact match {
        case Some(m) => value == m
        case _ => false
      }
      case _ => false
    }
  }

  override def hashCode(): Int = {
    Try(value.bigInteger.intValueExact()) match {
      case Success(n) => n
      case _ => Try(value.bigInteger.longValueExact()) match {
        case Success(n) => (n ^ (n >>> 32)).toInt
        case _ => value.hashCode()
      }
    }
  }

}

object JsBigInt:
  val prism: Prism[JsValue, BigInt] =
    Prism((value: JsValue) => value match
      case JsBigInt(value) => Some(value)
      case JsLong(value) => Some(BigInt(value))
      case JsInt(value) => Some(BigInt(value))
      case _ => None
    
    )(JsBigInt(_))



/**
 * represents an immutable boolean
 *
 * @param value the value associated, either true or false
 */
final case class JsBool(value: Boolean) extends JsPrimitive:
  override def toString: String = value.toString


object JsBool:

  val FALSE: JsBool = JsBool(false)

  val TRUE: JsBool = JsBool(true)

  val prism: Prism[JsValue, Boolean] =
    Prism((value: JsValue) => value match {
      case bool: JsBool => Some(bool.value)
      case _ => None
    })((d: Boolean) => JsBool(d))


sealed trait Json[T <: Json[T]] extends JsValue {

  def getArray(path: JsPath, default: => JsArray): JsArray = apply(path) match
    case array: JsArray => array
    case _ => default

  def getArray(path: JsPath): JsArray | Null = apply(path) match
    case array: JsArray => array
    case _ => null

  def getObj(path: JsPath, default: => JsObj): JsObj = apply(path) match
    case obj: JsObj => obj
    case _ => default

  def getObj(path: JsPath): JsObj | Null = apply(path) match
    case obj: JsObj => obj
    case _ => null

  def getLong(path: JsPath, default: => Long): Long = apply(path) match
    case JsLong(n) => n
    case JsInt(n) => n
    case _ => default

  def getLong(path: JsPath): Long | Null = apply(path) match
    case JsLong(n) => n
    case JsInt(n) => n
    case _ => null

  def getInt(path: JsPath, default: => Int): Int = apply(path) match
    case JsInt(n) => n
    case _ => default

  def getInt(path: JsPath): Int | Null = apply(path) match
    case JsInt(n) => n
    case _ => null

  def getInstant(path: JsPath): Instant | Null = apply(path) match
    case JsInstant(i) => i
    case JsStr(s) => JsStr.instantPrism.getOption(s) match
      case Some(i) => i
      case None => null
    case _ => null

  def getInstant(path: JsPath, default: => Instant): Instant = getInstant(path) match
    case i: Instant => i
    case null => default
  def getDouble(path: JsPath, default: => Double): Double = apply(path) match
    case JsInt(n) => n
    case JsLong(n) => n.toDouble
    case JsDouble(n) => n
    case _ => default

  def getDouble(path: JsPath): Double | Null = apply(path) match
    case JsInt(n) => n
    case JsLong(n) => n.toDouble
    case JsDouble(n) => n
    case _ => null

  def getNumber(path: JsPath, default: => BigDecimal): BigDecimal = apply(path) match
    case JsInt(n) => BigDecimal(n)
    case JsLong(n) => BigDecimal(n)
    case JsDouble(n) => BigDecimal(n)
    case JsBigDec(n) => n
    case JsBigInt(n) => BigDecimal(n)
    case _ => default

  def getNumber(path: JsPath): BigDecimal | Null = apply(path) match
    case JsInt(n) => BigDecimal(n)
    case JsLong(n) => BigDecimal(n)
    case JsDouble(n) => BigDecimal(n)
    case JsBigDec(n) => n
    case JsBigInt(n) => BigDecimal(n)
    case _ => null

  def getIntegral(path: JsPath, default: => BigInt): BigInt = apply(path) match
    case JsInt(n) => BigInt(n)
    case JsLong(n) => BigInt(n)
    case JsBigInt(n) => n
    case _ => default

  def getIntegral(path: JsPath): BigInt | Null = apply(path) match
    case JsInt(n) => BigInt(n)
    case JsLong(n) => BigInt(n)
    case JsBigInt(n) => n
    case _ => null

  def getBool(path: JsPath, default: => Boolean): Boolean = apply(path) match
    case JsBool(n) => n
    case _ => default

  def getBool(path: JsPath): Boolean | Null = apply(path) match
    case JsBool(n) => n
    case _ =>  null

  def getStr(path: JsPath, default: => String): String = apply(path) match
    case JsStr(n) => n
    case _ => default

  def getStr(path: JsPath): String | Null = apply(path) match
    case JsStr(n) => n
    case _ => null

  /** Converts the string representation of this Json to a pretty print version
   *
   * @return pretty print version of the string representation of this Json
   */
  def toPrettyString(config: WriterConfig=ParserConf.DEFAULT_WRITER_CONFIG):String

  /** Returns the string representation of this Json
   *
   * @return the string representation of this Json
   */
  override def toString: String 



  /**
   * Returns a zero-argument function that when called, it serializes this Json into the given
   * output stream, no returning anything
   *
   * @param outputStream the output stream
   * @return () => Unit function that serializes this Json into the given output stream
   */
  def serialize(outputStream: OutputStream): () => Unit = serialize(outputStream,ParserConf.DEFAULT_WRITER_CONFIG)

  def serialize(outputStream: OutputStream,writerConfig: WriterConfig): () => Unit


  /** Serialize this Json into an array of bytes. When possible,
   * it's more efficient to work on byte level that with strings
   *
   * @return this Json serialized into an array of bytes
   */
  def serialize(config:WriterConfig=ParserConf.DEFAULT_WRITER_CONFIG): Array[Byte]



  /** Removes a path from this Json
   *
   * @param path the path to be removed
   * @return If this Json does not contain a binding for path it is returned unchanged.
   *         Otherwise, returns a new Json without a binding for path
   */
  def removed(path: JsPath): T

  /** Returns the element located at a specified path. This function is total on its argument.
   * If no element is found, JsNothing is returned
   *
   * @param path the path
   * @return the json value found at the path
   */
  @scala.annotation.tailrec
  final def apply(path: JsPath): JsValue =
    if path.nn.isEmpty then this
    else if path.tail.isEmpty then apply(path.head)
    else apply(path.head) match
      case json: Json[_] => json.apply(path.tail)
      case _ => JsNothing


  /** returns true if the Json is empty
   *
   * @return true if empty, false otherwise
   */
  def isEmpty: Boolean

  /** returns true if the Json is non empty
   *
   * @return true if non empty, false otherwise
   */
  final def nonEmpty: Boolean = !isEmpty

  def flatten: LazyList[(JsPath, JsValue)]

  /** The initial part of the Json object without its last element.
   */
  def init: T

  /** The rest of the Json object without its first element. */
  def tail: T

  def size: Int

  /** Selects all the values of this Json which satisfy a predicate and are not Jsons. When a Json is
   * found, it is filtered recursively.
   *
   * @param p the predicate uses to test elements. The predicate accepts the path/json.value pair of each element
   * @return a new Json  consisting of all elements of this
   *         Json that satisfy the given predicate p.
   */
  def filter(p: (JsPath, JsPrimitive) => Boolean): T


  /** Selects all the values of this Json which satisfy a predicate and are not Jsons. When a Json is
   * found, it is filtered recursively.
   *
   * @param p the predicate uses to test elements. The predicate accepts the json.value of each element
   * @return a new Json  consisting of all elements of this
   *         Json that satisfy the given predicate p.
   */
  def filter(p: JsPrimitive => Boolean): T


  /**
   * Builds a new Json by applying a function to all elements of this Json that are not Json and satisfies a
   * given predicate. When a Json is found, it it mapped recursively.
   *
   * @param m the function to apply to each element. The predicate accepts the path/json.value pair of each element
   * @param p filter to select which elements will be mapped. By default all the elements are selected.
   * @return a new Json resulting from applying the given map function to each element of this Json that satisfies the filter
   *         and collecting the results.
   */
  def map(m: (JsPath, JsPrimitive) => JsValue,
          p: (JsPath, JsPrimitive) => Boolean
         ): T


  /**
   * Builds a new Json by applying a function to all elements of this Json that are not Json.
   * When a Json is found, it it mapped recursively.
   *
   * @param m the function to apply to each element. It accepts the json.value of each element
   * @return a new Json resulting from applying the given map function to each element of this Json that satisfies the filter
   *         and collecting the results.
   */
  def map(m: JsPrimitive => JsValue): T


  /**
   * Builds a new Json by applying a function to all the keys of this Json that satisfies a given predicate.
   * If the element associated to a key is a Json, the function is applied recursively,
   *
   * @param m the function to apply to each key. It accepts the path/json.value pair as parameters
   * @param p the predicate to select which keys will be mapped
   * @return
   */
  def mapKeys(m: (JsPath, JsValue) => String,
              p: (JsPath, JsValue) => Boolean
             ): T


  /**
   * Builds a new Json by applying a function to all the keys of this Json.
   * If the element associated to a key is a Json, the function is applied recursively,
   *
   * @param m the function to apply to each key. It accepts the key name as a parameter
   */
  def mapKeys(m: String => String): T


  def reduce[V](p: (JsPath, JsPrimitive) => Boolean,
                m: (JsPath, JsPrimitive) => V,
                r: (V, V) => V
               ): Option[V]

  /** Removes all the Json object of this Json which dont' satisfy a predicate. When a Json is
   * found, it is filtered recursively (if it passes the filter).
   *
   * @param p the predicate uses to test the path/object pairs.
   * @return a new Json consisting of all its elements except those
   *         Json object that dont satisfy the given predicate p.
   */
  def filterJsObj(p: (JsPath, JsObj) => Boolean): T

  /** Removes all the Json object of this Json which dont' satisfy a predicate. When a Json is
   * found, it is filtered recursively (if it passes the filter).
   *
   * @param p the predicate uses to test the Json object.
   * @return a new Json consisting of all its elements except those
   *         Json object that dont satisfy the given predicate p.
   */
  def filterJsObj(p: JsObj => Boolean): T

  /** Removes all the keys of this Json which dont' satisfy a predicate. When a Json is
   * found, it is filtered recursively.
   *
   * @param p the predicate uses to test the path/json.value pairs.
   * @return a new Json consisting of all array elements of this
   *         Json and those key/json.value pairs that satisfy the given predicate p.
   */
  def filterKeys(p: (JsPath, JsValue) => Boolean): T


  /** Removes all the keys of this Json which dont' satisfy a predicate. When a Json is
   * found, it is filtered recursively.
   *
   * @param p the predicate uses to test the keys.
   * @return a new Json consisting of all array elements of this
   *         Json and those key/json.value pairs that satisfy the given predicate p.
   */
  def filterKeys(p: String => Boolean): T


  /** Creates a new Json obtained by inserting a given path/json.value pair into this Json.
   * The given element is always inserted at the given path, even if it requires to create new Json
   * or padding arrays.
   *
   * @param path  the path
   * @param value the json.value
   * @return A new Json  with the new path/json.value mapping added to this Json.
   */
  def updated(path: JsPath,
              value: JsValue,
              padWith: JsValue = JsNull
              ): T

  private[json] def apply(pos: Position): JsValue
}

object Json:
  val prism: Prism[JsValue, Json[_]] =
    Prism[JsValue, Json[_]]((value: JsValue) => value match
    {
      case obj: JsObj => Some(obj)
      case arr: JsArray => Some(arr)
      case _ => None
    })((json: Json[_]) => json)


/**
 * represents an immutable Json object. There are several ways of creating a Json object, being the most
 * common the following:
 *
 *  - From a string, array of bytes or an input stream of bytes, using the parse functions of the companion object
 *  - From the apply function of the companion object.
 *
 * @param bindings immutable map of JsValue
 */
final case class JsObj(override val bindings: immutable.Map[String, JsValue] = HashMap.empty)
  extends AbstractJsObj(bindings)
    with IterableOnce[(String, JsValue)]
    with Json[JsObj] {

  /**
   * string representation of this Json array. It's a lazy json.value which is only computed once.
   *
   * @return string representation of this Json array
   */
  override def toString: String = writeToString(this)(JsObj.defaultCodec)
  
  override def serialize(config: WriterConfig): Array[Byte] = 
    writeToArray(this,config)(JsObj.defaultCodec)

  override def serialize(outputStream: OutputStream,config: WriterConfig): () => Unit = 
    () => writeToStream(this,outputStream,config)(JsObj.defaultCodec)
  
  override def toPrettyString(config: WriterConfig): String =
    writeToString(this,config)(JsObj.defaultCodec)

  def removed(key:String) = JsObj(bindings.removed(key))

  def removedAll(keys:IterableOnce[String]) =
    JsObj(bindings.removedAll(keys))

  override def removed(path: JsPath): JsObj =
    if (path.isEmpty) return this
    path.head match
      case Index(_) => this
      case Key(k) => path.tail match
        case JsPath.`root` => JsObj(bindings.removed(k))
        case tail => tail.head match
          case Index(_) => bindings.get(k) match
            case Some(a: JsArray) => JsObj(bindings.updated(k, a.removed(tail)))
            case _ => this
          case Key(_) => bindings.get(k) match
            case Some(o: JsObj) => JsObj(bindings.updated(k, o.removed(tail)))
            case _ => this

  def concat(other: JsObj): JsObj =
    if other.isEmpty then this
    else if isEmpty then other
    else JsObj(bindings concat other.bindings)


  def updated(key: String,
              value: JsValue
              ): JsObj = JsObj(bindings.updated(key, value))

  override def updated(path: JsPath,
                       value: JsValue,
                       padWith: JsValue = JsNull
                       ): JsObj =
    if path.nn.isEmpty then return this
    if value == JsNothing then return this.removed(path)
    path.head match
      case Index(_) => this
      case Key(k) => path.tail match
        case JsPath.`root` => JsObj(bindings.updated(k, value))
        case tail => tail.head match
          case Index(_) => bindings.get(k) match
            case Some(a: JsArray) => JsObj(bindings.updated(k,a.updated(tail, value, padWith)))
            case _ => JsObj(bindings.updated(k, JsArray.empty.updated(tail, value, padWith)))
          case Key(_) => bindings.get(k) match
            case Some(o: JsObj) => JsObj(bindings.updated(k, o.updated(tail, value, padWith)))
            case _ => JsObj(bindings.updated(k, JsObj().updated(tail, value, padWith)))


  override def equals(that: Any): Boolean =
    if that == null then false
    else that match
      case JsObj(m) => m == bindings
      case _ => false

}


/**
 * represents an immutable Json array. There are several ways of creating a Json array, being the most
 * common the following:
 *
 *  - From a string, array of bytes or an input stream of bytes, using the parse functions of the companion object
 *  - From the apply function of the companion object:
 *
 * @param seq immutable seq of JsValue
 */
final case class JsArray(override val seq: immutable.Seq[JsValue] = Vector.empty)
  extends AbstractJsArray(seq)
    with IterableOnce[JsValue]
    with Json[JsArray] {

  override def toString: String =
    writeToString(this)(JsArray.defaultCodec)

  def updated(index: Int, value: JsValue): JsArray =
    updated(JsPath.root / index, value)

  def appended(value: JsValue): JsArray =
    value match
      case JsNothing => this
      case _ => JsArray(seq.appended(value))

  def prepended(value: JsValue): JsArray =
    value match
      case JsNothing => this
      case _ => JsArray(seq.prepended(value))

  override def updated(path: JsPath,
                       value: JsValue,
                       padWith: JsValue = JsNull
                       ): JsArray =
    if path.nn.isEmpty then return this
    value match
      case JsNothing => this
      case _ => path.head match
        case Key(_) => this
        case Index(i) => path.tail match
          case JsPath.root => JsArray(fillWith(seq, i, value, padWith))
          case tail: JsPath => tail.head match
            case Index(_) => seq.lift(i) match
              case Some(a: JsArray) => JsArray(fillWith(seq, i, a.updated(tail, value, padWith), padWith))
              case _ => JsArray(fillWith(seq, i, JsArray.empty.updated(tail, value, padWith), padWith))
            case Key(_) => seq.lift(i) match
              case Some(o: JsObj) => JsArray(fillWith(seq, i, o.updated(tail, value, padWith), padWith))
              case _ => JsArray(fillWith(seq, i, JsObj().updated(tail, value, padWith), padWith))



  override def removed(path: JsPath): JsArray =
    if path.nn.isEmpty then return this
    path.head match
      case Key(_) => this
      case Index(i) => path.tail match
        case JsPath.`root` => JsArray(AbstractJsArray.remove(i, seq))
        case tail: JsPath => tail.head match
          case Index(_) => seq.lift(i) match
            case Some(a: JsArray) => JsArray(seq.updated(i, a.removed(tail)))
            case _ => this
          case Key(_) => seq.lift(i) match
            case Some(o: JsObj) => JsArray(seq.updated(i, o.removed(tail)))
            case _ => this

  override def equals(that: Any): Boolean =
    if that == null then false
    else that match
      case JsArray(m) => m == seq
      case _ => false

  override def serialize(config: WriterConfig): Array[Byte] =
    writeToArray(this, config)(JsArray.defaultCodec)

  override def serialize(outputStream: OutputStream, config: WriterConfig): () => Unit = 
    () => writeToStream(this, outputStream, config)(JsArray.defaultCodec)

  override def toPrettyString(config: WriterConfig): String =
    writeToString(this,config)(JsArray.defaultCodec)
  
}

/**
 * It's a special Json value that represents 'nothing'. Inserting nothing in a json leaves the json
 * unchanged. Functions that return a [[JsValue]], return JsNothing when no element is found, what makes
 * them total on their arguments.
 *
 * val obj = JsObj.empty
 * obj("a") == JsNothing
 * obj.inserted("a",JsNothing) == obj
 */
case object JsNothing extends JsValue

/**
 * Json null singleton object
 */
case object JsNull extends JsPrimitive

object JsObj:

  val lens = JsObjLenses

  val optional = JsObjOptionals


  val empty: JsObj = JsObj(immutable.HashMap.empty)

  private[json] val defaultCodec = JsObjCodec(JsObjParser.DEFAULT)
  
  def parse(json:String):JsObj = 
    readFromString(json,ParserConf.DEFAULT_READER_CONFIG)(defaultCodec)

  def parse(decimalConf: DecimalConf,
            bigIntDigitsLimit:Int,
            config: ReaderConfig)(json:String):JsObj =
    readFromString(json,config)(JsObjCodec(JsObjParser(decimalConf,bigIntDigitsLimit)))
  def parse(json:Array[Byte]):JsObj = 
    readFromArray(json,ParserConf.DEFAULT_READER_CONFIG)(defaultCodec)

  def parse(decimalConf: DecimalConf,
            bigIntDigitsLimit:Int,
            config: ReaderConfig)(json: Array[Byte]): JsObj =
    readFromArray(json,config)(JsObjCodec(JsObjParser(decimalConf,bigIntDigitsLimit)))

  def apply(pairs: (String, JsValue)*): JsObj =
    @scala.annotation.tailrec
    def applyRec(acc: JsObj,
                 pair: Seq[(String, JsValue)]
                ): JsObj =
      if pair.isEmpty then acc
      else applyRec(acc.updated(pair.head._1, pair.head._2), pair.tail)

    applyRec(empty, pairs)

  def pairs(pairs: (JsPath, JsValue)*): JsObj =
    if pairs.count(pair => pair._1.head match
      case Index(_) => true
      case _ => false) > 0
    then throw UnsupportedOperationException("head of a path is an index")
    else fromPairsRec[JsObj](JsObj.empty, pairs)

  val prism: Prism[JsValue, JsObj] =
    Prism((value: JsValue) => value match {
      case obj: JsObj => Some(obj)
      case _ => None
    })((d: JsObj) => d)


object JsArray:
  val lens = JsArrayLenses

  val optional = JsArrayOptionals


  val empty: JsArray = JsArray(Vector.empty)
  
  private[json] val defaultCodec =
    JsArrayCodec(JsArrayOfParser(JsValueParser.DEFAULT))

  def parse(json:String) =
    readFromString(json,ParserConf.DEFAULT_READER_CONFIG)(defaultCodec)

  def parse(decimalConf: DecimalConf,
            bigIntDigitsLimit:Int,
            config: ReaderConfig)(json:String) =
    readFromString(json,config)(JsArrayCodec(JsArrayOfParser(JsValueParser(decimalConf,bigIntDigitsLimit))))

  def parse(json:Array[Byte]) =
    readFromArray(json,ParserConf.DEFAULT_READER_CONFIG)(defaultCodec)

  def parse(decimalConf: DecimalConf,
            bigIntDigitsLimit:Int,
            config: ReaderConfig)(json:Array[Byte]) =
    readFromArray(json,config)(JsArrayCodec(JsArrayOfParser(JsValueParser(decimalConf,bigIntDigitsLimit))))

  def apply(pair: (JsPath, JsValue),
            xs: (JsPath, JsValue)*
           ): JsArray =
    @scala.annotation.tailrec
    def apply0(arr: JsArray,
               seq: Seq[(JsPath, JsValue)]
              ): JsArray =
      if seq.isEmpty then arr
      else apply0(arr.updated(seq.head._1, seq.head._2), seq.tail)
    apply0(empty.updated(pair._1, pair._2), xs)

  def apply(value: Int,
            values: Int*
           ): JsArray = JsArray(values.map(JsInt.apply)).prepended(JsInt(value))

  def apply(value: Long,
            values: Long*
           ): JsArray = JsArray(values.map(JsLong.apply)).prepended(JsLong(value))

  def apply(value: String,
            values: String*
           ): JsArray = JsArray(values.map(JsStr.apply)).prepended(JsStr(value))

  def apply(value: Double,
            values: Double*
           ): JsArray = JsArray(values.map(JsDouble.apply)).prepended(JsDouble(value))

  def apply(value: BigDecimal,
            values: BigDecimal*
           ): JsArray = JsArray(values.map(JsBigDec.apply)).prepended(JsBigDec(value))

  def apply(value: BigInt,
            values: BigInt*
           ): JsArray = JsArray(values.map(JsBigInt.apply)).prepended(JsBigInt(value))

  def apply(value: Boolean,
            values: Boolean*
           ): JsArray = JsArray(values.map(JsBool.apply)).prepended(JsBool(value))


  def apply(value: JsValue,
            values: JsValue*
           ): JsArray = JsArray(values).prepended(value)

  def pairs(pairs: (JsPath, JsValue)*): JsArray =
    if pairs.count(pair => pair._1.head match
      case Key(_) => true
      case Index(_) => false) > 0
    then throw UnsupportedOperationException("head of a path is a key")
    else fromPairsRec[JsArray](JsArray.empty, pairs)

  val prism: Prism[JsValue, JsArray] =
    Prism((value: JsValue) => value match {
      case arr: JsArray => Some(arr)
      case _ => None
    })((arr: JsArray) => arr)


@scala.annotation.tailrec
private[value] def fromPairsRec[T <: Json[T]](acc: T,
                                            pairs: Seq[(JsPath, JsValue)]
                                           ): T =
  if pairs.isEmpty then acc
  else
    val (key, value) = pairs.head
    fromPairsRec(acc updated(key, value), pairs.tail)