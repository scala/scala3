
object Test {

  def main(args: Array[String]): Unit = {
    val i = I(2)
    E.eval(
      i // error
    )

    E.eval(Plus(
      i, // error
      I(4)))

    val plus = Plus2.IPlus
    E.eval(Plus(I(2), I(4))(
      plus // error
    ))
  }

}
