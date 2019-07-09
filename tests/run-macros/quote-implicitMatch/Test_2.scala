object Test extends App {

  implicitly[Ordering[String]]

  println(f1[String]().getClass)
  println(f1[AnyRef]().getClass)

  implicit val b: B = new B
  implicitly[B]
  g

}