object Test {
  val li = List(1, 2, 3).foldLeft(Nil)((acc, x) => x :: acc)
  val cli: List[Int] = li

  val li2 = List(1, 2, 3).foldLeft(Nil)((acc, x) => acc)
  val cli2: List[Int] = li2

  val ld = List(1, 2, 3).foldLeft(Nil)((acc, x) => x.toDouble :: acc)
  val cld: List[Double] = ld

  val si = Seq(1, 2, 3).foldLeft(Set())((acc, x) => acc + x)
  val csi: Set[Int] = si

  val si2 = Seq(1, 2, 3).foldLeft(Set())((acc, x) => acc ++ Set(x))
  val csi2: Set[Int] = si2

  val ssi = Seq(1, 2, 3).foldLeft(Set())((acc, x) => acc + Set(x))
  val cssi: Set[Set[Int]] = ssi
}

object NotWorking {
  /*
  val ld2 = List(1, 2, 3).foldLeft(Nil)((acc, x) => {
    val y = x.toDouble :: acc
    y // error: found: List[Double], required: List[Nothing]
  })
  val cld2: List[Double] = ld2
  */
}
