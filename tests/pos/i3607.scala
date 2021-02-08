object A { implicit val x: Int = 1 }

object Test1 {
  import A.x as y
  implicitly[Int]
}
