val xs: List[Any] = ???
val as = for (x: String) <- xs yield x
val bs =
  for
    (x: String) <- xs
  yield x
