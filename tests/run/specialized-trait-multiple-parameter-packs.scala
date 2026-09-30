//> using options -language:experimental.specializedTraits

inline trait A[T: Specialized, S: Numeric](val x: T)(val y: T)(val z: S)(p: S):
    val p2 = p
    def getArgs = (x, y, z, p2)

class B(w: Int)(e: String) extends A[String, Int](e)("Y")(100)(w)

@main def Test = 
    val b = B(41)("Good Morning")
    assert(b.getArgs == ("Good Morning", "Y", 100, 41))

    val a = new A[Boolean, Long](true)(false)(1000000000)(-10) {}
    assert(a.getArgs == (true, false, 1000000000, -10))
