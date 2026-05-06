package ttt
object Test uses A, Console:
  def test =
    val leak: String -> Unit = (secret: String) => // error
      A.println(secret)
