object Test {

  val o: Object = "A"

  println(o.isInstanceOf[Int])

  val i: Int = o.asInstanceOf[Int]

  val o2 = i.asInstanceOf[Object]

}

