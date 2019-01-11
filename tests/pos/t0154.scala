package test
trait MyMatchers  {
  val StringMatch = new AnyRef {}
    trait Something {
      (null : AnyRef|Null) match {
        case (StringMatch) =>
        case _ =>
      }
   }
}
