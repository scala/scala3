object Test {
  sealed trait Off
  case class Of[sup, sub <: sup]() extends Off

  def sup[O <: Off](o: O) = o match {
    case _: Of[sup, sub] =>
      val a: sub = null.asInstanceOf
      // Even though we know that sub <: sup from Of's bounds, that knowledge
      // is lost in the body of pattern matching expressions...

      val b: sup = a // error
      //  Found:    (a : sub)
      //  Required: sup
      //  where:    sub is a type in method sup with bounds <: Test.Of[?, ?]#sup

      ()
  }
}
