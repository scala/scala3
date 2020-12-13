class Bag extends reflect.Selectable

@main def Test =
  val x = new Bag:
    val f1 = 23

  println(x.f1())  // error