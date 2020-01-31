object Test extends App {
  given f[H](given h: H) as H = h
  summon[Int]  // error
}