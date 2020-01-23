object Test extends App {
  given f[H] with (h: H) as H = h
  summon[Int]  // error
}