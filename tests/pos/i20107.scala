object foo:
	transparent inline def unapply[F](e: F): Option[F] = Some(e.asInstanceOf[F])

class A:
  def test(x: Int) = x match
    case foo(e) => e
