import collection.IndexedSeqView
object Hello extends App {
  def foo(view: IndexedSeqView[Int]): Unit =
    val x1 = 1 +: view
    val x2 = view :+ 1
}

