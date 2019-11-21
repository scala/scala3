package example

class ValPattern {

  val (left, right) = (1, 2)
  val Some(number1) =
    Some(1)

  val List(Some(q1), None: None.type, None) = ???

  var (leftVar, rightVar) = (1, 2)
  var Some(number1Var) =
    Some(1)

  def app(): Unit = {
    println(
      (
        number1,
        left,
        right,
        number1Var,
        leftVar,
        rightVar
      )
    )
    locally {
      val (left, right) = (1, 2)
      val Some(number1) =
        Some(1)

      var (leftVar, rightVar) = (1, 2)
      var Some(number1Var) =
        Some(1)
      println(
        (
          number1,
          left,
          right,
          number1Var,
          leftVar,
          rightVar
        )
      )
    }
  }

}
