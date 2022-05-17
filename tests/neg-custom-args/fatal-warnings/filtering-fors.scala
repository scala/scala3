object Test {

  val xs: List[AnyRef] = ???

  for (x <- xs) do ()   // OK
  for (x: Any <- xs) do () // OK

  for (x: String <- xs) do ()      // error
  for ((x: String) <- xs) do ()    // error
  for (y@ (x: String) <- xs) do () // error
  for ((x, y) <- xs) do ()         // error

  for ((x: String) <- xs if x.isEmpty) do ()   // error
  for ((x: String) <- xs; y = x) do ()          // error
  for ((x: String) <- xs; (y, z) <- xs) do ()   // error // error
  for (case (x: String) <- xs; (y, z) <- xs) do () // error
  for ((x: String) <- xs; case (y, z) <- xs) do () // error

  val pairs: List[AnyRef] = List((1, 2), "hello", (3, 4))
  for ((x, y) <- pairs) yield (y, x) // error

  for (case x: String <- xs) do ()      // OK
  for (case (x: String) <- xs) do ()    // OK
  for (case y@ (x: String) <- xs) do () // OK
  for (case (x, y) <- xs) do ()         // OK

  for (case (x: String) <- xs if x.isEmpty) do ()   // OK
  for (case (x: String) <- xs; y = x) do ()          // OK
  for (case (x: String) <- xs; case (y, z) <- xs) do ()   // OK

  for (case (x, y) <- pairs) yield (y, x) // OK
}