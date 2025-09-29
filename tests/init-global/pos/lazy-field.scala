object O {
  lazy val f1 = this
  val f2 = 5
  val f3 = f1.f2 + 5
}