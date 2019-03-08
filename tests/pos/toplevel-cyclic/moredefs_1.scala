class B(x: Option[A])

val r = new B(Some(A(new B(None))))
