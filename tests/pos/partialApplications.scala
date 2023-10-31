object Test {

  type Histogram[X] = Map[X, Int]

  type StringlyHistogram[X >: String] = Histogram[X]

  val xs: Histogram[String] = Map[String, Int]()

  val ys: StringlyHistogram[String] = xs

  def e = xs

  val zs: StringlyHistogram[?] = e

  type IntMap[Y] = Map[Int, Y]

  val is = Map[Int, Boolean]()

  val js: IntMap[Boolean] = is

  val ks: IntMap[?] = is

  type RMap[X, Y] = Map[Y, X]

  val rs = Map[Int, Float]()

  val ss: RMap[Float, Int] = rs

}
