//> using options -Yexplicit-nulls
import language.experimental.errorHandling
trait Generic extends Selectable:
  def applyDynamic(name: String)(args: Any*): Any = ()

val foo: Generic {
  def unapply(x: Int): Unit?
} = new Generic:
  def unapply(x: Int): Unit? = ()

def x =
  42 match
    case foo(()) => println("lol") // error

