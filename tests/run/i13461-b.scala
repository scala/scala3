object Time:
  opaque type Time = String
  opaque type Seconds <: Time = String
  opaque type Minutes <: Time = String
  opaque type Mixed <: Time = String

  type Units = Seconds | Minutes

  def sec(n: Int): Seconds =
    s"${n}s"

  def min(n: Int): Minutes =
    s"${n}m"

  def mixed(t1: Time, t2: Time): Mixed =
    s"${t1}, ${t2}"

  extension (t: Units)
    def number: Int =
      (t : String).init.toInt

  extension [T1 <: Time](inline a: T1)
    transparent inline def +[T2 <: Time](inline b: T2): Time =
      inline (a, b) match
        case x: (Seconds, Seconds) =>
          (sec(x._1.number + x._2.number))

        case x: (Minutes, Minutes) =>
          (min(x._1.number + x._2.number))

        case x: (Time, Time) =>
          (mixed(x._1, x._2))
    end +
end Time

import Time.*

// Test seconds
val a = sec(15)
val b = sec(20)

// Test minutes
val x = min(15)
val y = min(20)

// Test mixes
val m1 = a + y
val m2 = x + b

// Test upper type
val t1: Time = a
val t2: Time = x
val t3: Time = m1

@main def Test() =
  println(a + b)
  println(x + y)
  println(m1 + m2)
  println(t1 + t2 + t3)
