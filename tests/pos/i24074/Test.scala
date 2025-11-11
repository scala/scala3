// trait Test extends JavaPart.Lvl3
// object TestImpl extends Test:
//   def getData(): Array[JavaPart.B] = ???
object Test:
  def test(lvl3: JavaPart.Lvl3): Unit =
    lvl3.getData.head.onlyInB()