/** A test that shows how to use shapeless.Typeable in extractors without the
 *  TypeLevel Scala 4 extensions.
 *  In essence you have to write
 *
 *      case Typeable.instanceOf[T](x) =>
 *
 *  instead of
 *
 *      case Typeable[T](x)
 *
 *  The first idiom would be nice to have but it requires more backtracking
 *  in Typer that we allow now. Essentially, given
 *
 *      case C[T](x)
 *
 *  it's unclear whether this should expand to `C[T].unapply(x)`, (as it does now)
 *  or to `C.unapply[T](x)` (which is what TypeLevel Scala 4 did, I believe)
 */
trait Typeable[T]:
  def cast(x: Any): Option[T]
  def describe: String
  override def toString = s"Typeable[$describe]"

object Typeable:
  def apply[T: Typeable]: Typeable[T] = summon

  class instanceOf[T: Typeable]:
    def unapply(x: Any): Option[T] = Typeable[T].cast(x)

  given int: Typeable[Int]:
    def cast(x: Any): Option[Int] = x match
      case x: Int => Some(x)
      case _ => None
    def describe = "Int"

  given list: [T: Typeable] => Typeable[List[T]]:
    def cast(x: Any): Option[List[T]] = x match
      case x: List[_] if x.forall(Typeable[T].cast(_).isDefined) => Some(x.asInstanceOf[List[T]])
      case _ => None
    def describe = s"List[${Typeable[T].describe}]"
end Typeable

def testInstance[T: Typeable](x: Any): Unit =
  val isa = x match
    case Typeable.instanceOf[T](x) => "is a"
    case _ => "is not a"
  println(s"$x $isa ${Typeable[T].describe}")

@main def Test() =
  testInstance[Int](1)
  testInstance[List[Int]](List(1, 2, 3))
  testInstance[Int](List(1, 2, 3))
  testInstance[List[Int]](1)
  testInstance[Int]("a")
  testInstance[List[Int]](Nil)
  testInstance[List[Int]](1 :: "a" :: Nil)
  testInstance[List[Int]](1 :: 2 :: Nil)
