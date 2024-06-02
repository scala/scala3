import scala.language.experimental.namedTuples

object Test1:
  // original code from issue https://github.com/scala/scala3/issues/20439
  val bar = (a = 1, b = 2)

  type ThatBar = bar.type
  val thatBar: ThatBar = bar
  val thatBar2: bar.type = bar

  def test2 = thatBar.a // error
  def test3 = thatBar2.a // ok
