object Test extends App {
  import collection.mutable.ListBuffer

  def newLB = ListBuffer(Symbol("a"), Symbol("b"), Symbol("c"), Symbol("d"), Symbol("e"))

  def iiobe[A](f: => A) =
    try { f }
    catch { case ex: IndexOutOfBoundsException => println(ex) }

  val lb0 = newLB
  iiobe( lb0.insert(-1, Symbol("x")) )

  val lb1 = newLB
  iiobe( lb1.insertAll(-2, Array(Symbol("x"), Symbol("y"), Symbol("z"))) )

  val lb2 = newLB
  iiobe( lb2.update(-3, Symbol("u")) )

  val lb3 = newLB
  iiobe( lb3.updated(-1, Symbol("u")) )
  iiobe( lb3.updated(5, Symbol("u")) )
}
