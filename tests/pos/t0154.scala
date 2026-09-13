package test
trait MyMatchers  {
  val StringMatch = new AnyRef {}
    trait Something {
      (??? : AnyRef) match {
        case (StringMatch) =>
        case _ =>
      }
   }
}
