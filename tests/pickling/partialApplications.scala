object PartialApplications {

  type Histogram = Map[_, Int]

  type StringlyHistogram = Histogram[_ >: String]

  val xs: Histogram[String] = Map[String, Int]()

  val ys: StringlyHistogram[String] = xs

  val zs: StringlyHistogram = xs

}
