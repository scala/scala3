package b

import a.A

object B {

  def formattedEnum(e: A): String = e match {
    case A.X => "X"
    case A.Y => "Y"
    case A.Z => "Z"
  }

  @main def test =
    assert(A.values.toList == List(A.X, A.Y, A.Z))
    assert(A.values.toList.map(formattedEnum) == List("X", "Y", "Z"))
}

