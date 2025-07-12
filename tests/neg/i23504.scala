def test =
  Seq.empty[[T] =>> () => ?].head() // error
  Seq.empty[[T] =>> Int => Int].head(1) // error