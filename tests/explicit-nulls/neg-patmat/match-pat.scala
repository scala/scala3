class Foo {

  val s: String = ???

  s match {
    case s: String => 100
    case _ => 200 // error: unreachable
  }

  sealed trait Animal
  case class Dog(name: String) extends Animal
  case object Cat extends Animal

  val a: Animal = ???

  a match {
    case Dog(name) => 100
    case Cat => 200
    case _ => 300 // error: unreachable
  }

  val a2: Animal | Null = ???

  a2 match {
    case Dog(_) => 100
    case Cat => 200
    case _ => 300
  }

  val a3: Animal | Null = ???

  a3 match {
    case Dog(_) => 100
    case Cat => 200
    case null => 300 // ok
  }
}
