object Hello extends App {

  val (a: Int, b: Int) = (4, 5) // works
  val (z, k): (4, 5) = (4, 5) // works
  val (x: 4, y: 5) = (4, 5) // doesn't work

}
