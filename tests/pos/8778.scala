trait Inv[T]

object Test {
  type M[X] = X match {
    case Inv[Int & String] => Int
      // the type test for Inv[Int & String] cannot be checked at runtime
      // BUT if it could, ...
    case Any => String
  }

  def m[X](x: X): M[X] = x match {
    case _: Inv[Int & String] => 1
    case _: Any => "s"
  }

  // Suppose we somehow manage to convince the compiler of that...
  val ev: Inv[Nothing] =:= Inv[Int & String] = (implicitly[Int =:= Int]).asInstanceOf

  val invN: Inv[Nothing] = new Inv[Nothing] {}
  m(invN)     // reduces to Int    both as a value and as a type
  m(ev(invN)) // reduces to String both as a value and as a type
}

