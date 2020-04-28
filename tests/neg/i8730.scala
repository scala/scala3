import reflect.Selectable.reflectiveSelectable

class Nat(val x: Int) {
  def get: Int = x
  def isEmpty = x < 0
}

val SomeExtractorBuilder: { def unapply(x: Int): Nat } = new {
  def unapply(x: Int): Nat = new Nat(x)
}


@main
def Test = 5 match {
  case SomeExtractorBuilder(n) => println(s"$n is a natural number") // error
  case _      => ()
}

