package json.value
import monocle.Prism
/**
 * represents a position in a Json. A JsPath is a list of positions.
 */
sealed trait Position


/**
 * represents a key in a Json object
 *
 * @param name name of the key
 */
final case class Key(name: String) extends Position:
  override def toString: String = name



object Key:
  val prism: Prism[Position, String] = Prism((pos: Position) => pos match {
    case Key(name) => Some(name)
    case _:Position => None
  })((name: String) => Key(name))

/**
 * represents an index in a Json array
 *
 * @param i the number of the index
 */
final case class Index(i: Int) extends Position:
  override def toString: String = Integer.toString(i)



object Index:
  val prism: Prism[Position, Int] = Prism((pos: Position) => pos match {
    case Index(n) => Some(n)
    case _:Key => None
  })((n: Int) => Index(n))
