object Test {

  type Histogram[X] = Map[X, Int]

  type StringlyHistogram[X >: String] = Histogram[X]

  val xs: Histogram[String] = Map[String, Int]()

  val ys: StringlyHistogram[String] = xs

  val zs: StringlyHistogram = xs

}
