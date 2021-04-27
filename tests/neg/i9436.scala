class Bag extends reflect.Selectable

@main def Test =
  val x = new Bag:
    def f1() = 23
    def f2(x: Int, y: Int) = x + y

  println(x.f1)    // error
  println(x.f2(1)) // error
