object O {
  lazy val f1 = this
  val f2 = 5
  val f3: Int = f1.f2 + f1.f3 // warn
}