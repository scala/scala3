import collection.mutable.ListBuffer
import compiletime.uninitialized
import scala.util.boundary, boundary.{Label, break}

/** Contains a delimited contination, which can be invoked with `r`esume` */
class Suspension[+R]:
  def resume(): R = ???
object Suspension:
  def apply[R](): Suspension[R] =
    ??? // magic, can be called only from `suspend`

/** Returns `fn(s)` where `s` is the current suspension to the boundary associated
 *  with the given label.
 */
def suspend[R, T](fn: Suspension[R] => T)(using Label[T]): Unit =
  ??? // break(fn(Suspension()))

/** Returns the current suspension to the boundary associated
 *  with the given label.
 */
def suspend[R]()(using Label[Suspension[R]]): Unit =
  suspend[R, Suspension[R]](identity)

/** A single method iterator */
abstract class Choices[+T]:
  def next: Option[T]

object Choices:
  def apply[T](elems: T*): Choices[T] =
    apply(elems.iterator)

  def apply[T](it: Iterator[T]) = new Choices[T]:
    def next = if it.hasNext then Some(it.next) else None
end Choices

/** A Label representikng a boundary to which can be returned
 *   - None,  indicating an mepty choice
 *   - a suspension with Option[T] result, which will be
 *     iterated over element by element in the boundary's result.
 */
type CanChoose[T] = Label[None.type | Suspension[Option[T]]]

/** A variable representing Choices */
class Ref[T](choices: => Choices[T]):

  /** Try all values of this variable. For each value, run the continuation.
   *  If it yields a Some result take that as the next value of the enclosing
   *  boundary. If it yields a Non, skip the element.
   */
  def each[R](using CanChoose[R]): T =
    val cs = choices
    suspend()
    cs.next.getOrElse(break(None))

end Ref

/** A prompt to iterate a compputation `body` over all choices of
 *  variables which it references.
 *  @return all results in a new `Choices` iterator,
 */
def choices[T](body: CanChoose[T] ?=> T): Choices[T] = new Choices:
  var stack: List[Suspension[Option[T]]] = Nil

  def next: Option[T] =
    boundary(Some(body)) match
      case s: Some[T] => s
      case None =>
        if stack.isEmpty then
          // no (more) variable choices encountered
          None
        else
          // last variable's choices exhausted; use next value of previous variable
          stack = stack.tail
          stack.head.resume()
      case susp: Suspension[Option[T]] =>
        // A new Choices variable was encountered
        stack = susp :: stack
        stack.head.resume()

@main def test: Choices[Int] =
  val x = Ref(Choices(1, -2, -3))
  val y = Ref(Choices("ab", "cde"))
  choices:
    val xx = x.each
    xx + (
      if xx > 0 then y.each.length * x.each
      else y.each.length
    )
    /* Gives the results of the following computations:
        1 + 2 * 1
        1 + 2 * -2
        1 + 2 * -3
        1 + 3 * 1
        1 + 3 * -2
        1 + 3 * -3
        -2 + 2
        -2 + 3
        -3 + 2
        -3 + 3
    */



