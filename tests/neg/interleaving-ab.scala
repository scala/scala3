
object Ab:
  given String = ""
  given Double = 0

  def illegal[A][B](x: A)(using B): B = summon[B] // error: Type parameter lists must be separated by a term or using parameter list

  def ab[A](x: A)[B](using B): B = summon[B]
  def test =
    ab[Int](0: Int) // error
