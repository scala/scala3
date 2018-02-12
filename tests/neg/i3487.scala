object Test {
  type &:[H, T] = Int
  val a: F[Int] { type X = Int &: String } = ??? // error
}
