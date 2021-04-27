object Test extends App {
  given f[H](using h: H): H = h
  summon[Int]  // error
}