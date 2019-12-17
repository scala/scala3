object Test extends App {
  given f[H]: (h: H) => H = h
  summon[Int]  // error
}