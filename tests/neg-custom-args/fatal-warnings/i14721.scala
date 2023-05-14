
class C:
  def op: Unit = println("op")
  def handler: Unit = println("handler")
  def test: Unit =
    try op
    catch case _: NullPointerException =>
    handler   // error
