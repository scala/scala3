object Test {
  val X = Seq(1, 2)

  for (X <- Seq(3, 4)) yield println(X)
}
