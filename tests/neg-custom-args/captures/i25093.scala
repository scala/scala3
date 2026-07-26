def oneOf[A](as: List[DummyImplicit ?=> A]): A =
  as(0) // error