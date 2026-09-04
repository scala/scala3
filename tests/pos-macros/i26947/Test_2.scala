package lib

@main def test =
  val x: MyType = MyMacro.create
  println(x)
