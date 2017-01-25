trait AnyRef {
  val StringMatch = new AnyRef {}
  trait Something { (AnyRef) match { case (StringMatch) => } }
}
