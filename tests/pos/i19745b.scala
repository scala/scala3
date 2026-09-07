object x {
  trait GoodNoParens[T]
}
export x.GoodNoParens as BadNoParens

object App extends App {
  new BadNoParens {}
  new BadNoParens() {}
  new x.GoodNoParens {}
}
