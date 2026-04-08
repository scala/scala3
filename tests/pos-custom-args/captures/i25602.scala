import language.experimental.safe

val e: Either[Int, String] = Left(42)
val x1: BigInt = BigInt(42)
val x2: Double = math.sqrt(42)
val xs = List(0, 1).sorted
val o1: Option[Int] = Some(42)
val o2: Option[Int] = None