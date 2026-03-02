trait Generic extends Selectable:
  def applyDynamic(name: String)(args: Any*): Any = ()

val foo: Generic {
  def unapply(x: Int): Option[Unit]
} = new Generic:
  def unapply(x: Int): Option[Unit] = Some(())

def x =
  42 match
    case foo(()) => println("lol") // error

