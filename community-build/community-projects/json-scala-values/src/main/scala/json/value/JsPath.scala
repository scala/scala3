package json.value
import json.value.Key
import scala.Conversion
import scala.collection.immutable.Vector



/**
 * Represents the full path location of an element in a json. The easiest way of creating a JsPath is.
 *
 * @param positions keys and/or indexes a path is made up of
 */
final case class JsPath(positions: Seq[Position]):
  def length: Int = positions.size
  def inc: JsPath =
    if isEmpty then throw UnsupportedOperationException("inc of an empty path")
    last match
      case Key(_) => throw UnsupportedOperationException(s"inc of $this. Last position is not an index")
      case Index(i) => init / (i + 1)



  /** Alias for `appended` */
  @`inline` def /(index: Int): JsPath = appended(index)

  def appended(i: Int): JsPath = JsPath(positions appended Index(i))

  /** Alias for `prepended` */
  @`inline` def \(index: Int): JsPath = prepended(index)

  def prepended(i: Int): JsPath = JsPath(positions prepended Index(i))

  /** Alias for `appended` */
  @`inline` def /(key: String): JsPath = appended(key)

  def appended(name: String): JsPath = JsPath(positions appended Key(name))

  /** Alias for `prepended` */
  @`inline` def \(key: String): JsPath = prepended(key)

  def prepended(key: String): JsPath = JsPath(positions prepended Key(key))

  @`inline` def /(path: JsPath): JsPath = appended(path)

  def appended(path: JsPath): JsPath = JsPath(positions ++ path.positions)

  @`inline` def \(path: JsPath): JsPath = prepended(path)

  def prepended(path: JsPath): JsPath = JsPath(path.positions ++: positions)

  def head: Position = positions.head

  def tail: JsPath = JsPath(positions.tail)

  def last: Position = positions.last
  
  def lastKey:String|Null = last match
    case Key(name) => name
    case _ => null
  
  def lastIndex:Int|Null = last match
    case Index(n) => n
    case _ => null

  def init: JsPath = JsPath(positions.init)

  def isEmpty: Boolean = positions.isEmpty

  override def toString: String = positions.mkString(" / ")


object JsPath:
  val root: JsPath = JsPath(Vector.empty)

  private[value] val MINUS_ONE: JsPath = JsPath(Vector(Index(-1)))