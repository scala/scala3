object Test extends App {
  List(1, 2, 3) map (_ match { case x => x + 1 })
  List((1, 2)) x (_ match { case (x, z) => x + z }) // error
}
