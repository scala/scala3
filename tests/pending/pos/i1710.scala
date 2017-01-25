object Hello extends App {

  val (a: Int, b: Int) = (4, 5) // works
  val (z, k): (4, 5) = (4, 5) // works

  val cd: (4, 5) = (4, 5)
  val c: 4 = cd._1
  val d: 5 = cd._2
  val (x: 4, y: 5) = (4, 5) // doesn't work

}
