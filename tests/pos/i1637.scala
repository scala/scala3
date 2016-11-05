object Main extends App {
  case class Foo(field: Option[String])
  val x: PartialFunction[Foo, Int] = { c =>
    c.field match {
      case Some(s) => 42
    }
  }
}
