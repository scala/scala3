object Time:
  opaque type Time = String
  opaque type Seconds <: Time = String

  transparent inline def sec(n: Double): Seconds =
    s"${n}s": Seconds // opaque type aliases have to be dealiased in nested calls, otherwise the resulting program might not be typed correctly

  transparent inline def testInference(): List[Time] =
    List(sec(5)) // infers List[String] and returns List[Time] & List[String], not List[Seconds]
  transparent inline def testGuarded(): List[Time] =
    List(sec(5)): List[Seconds] // returns List[Seconds]
  transparent inline def testExplicitTime(): List[Time] =
    List[Seconds](sec(5)) // returns List[Seconds]
  transparent inline def testExplicitString(): List[Time] =
    List[String](sec(5)) // returns List[Time] & List[String]

end Time

@main def main() =
  val t1: List[String] = Time.testInference() // returns List[Time.Time] & List[String]
  val t2: List[Time.Seconds] = Time.testGuarded() // returns List[Time.Seconds]
  val t3: List[Time.Seconds] = Time.testExplicitTime() // returns List[Time.Seconds]
  val t4: List[String] = Time.testExplicitString() // returns List[Time.Time] & List[String]
