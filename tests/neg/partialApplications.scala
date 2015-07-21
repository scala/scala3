object Test {

  type RMap[X, Y] = Map[Y, X]
  val m = Map[Int, String]()
  val ts: RMap[_, Int] = m // erorr // error
  val us: RMap[String, _] = m // error // error
  val vs: RMap[_, _] = m // error // error // error
  val zz: RMap = m // error

}

