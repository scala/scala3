object Test extends App {
  given f[H](using h: H) as H = h
  summon[Int]  // error
}