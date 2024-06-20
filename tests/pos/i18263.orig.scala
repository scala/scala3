sealed trait Scope
sealed trait Domain extends Scope
object Domain extends Domain

trait Baz[T]
def baz(using ck: Scope): Baz[ck.type] = ???

class Foo extends scala.reflect.Selectable:
  type TScope = Domain
  final protected given TScope = Domain

object ID:
  val internal1 = new Foo:
    val ii = new Foo:
      val x = baz
  val z = internal1.ii.x //error
