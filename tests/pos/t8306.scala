class Si8306 {
  def foo: Int = 123
  lazy val ext: Int =
      foo match {
          case idx if idx != -1 => 15
          case _ => 17
      }
}
