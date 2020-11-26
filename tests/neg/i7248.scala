object Test extends App {
  given [H] => (h: H) => H as f = h
  summon[Int]  // error
}