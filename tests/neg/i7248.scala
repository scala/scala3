object Test extends App {
  given f[H](given h: H): H = h
  summon[Int]  // error
}