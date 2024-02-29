class Foo
trait Bar:
  val foo : Int
  val f : Option[foo.type] = Some(foo)

  def g : Boolean =
    f match
      case None => false
      case Some(_) => true
