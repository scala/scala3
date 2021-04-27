extension (s: String) def times(i: Int): String = s * i

val partial = "abc".times

@main def Test =
  assert(partial(2) == "abcabc")
