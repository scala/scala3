//> using options -language:experimental.specializedTraits


inline trait A[T: Specialized, S: Numeric](val x: T)(val y: T)(val z: S)(p: S):
    def getArgs = (x, y, z, p)

inline def foo[P: Specialized](x: P)[Q: {Specialized, Numeric}](y: P, z: Q, p: Q) = new A[P, Q](x)(y)(z)(p) {}

inline trait B[T: Specialized](val w: T):
    inline def foo[P: Specialized](x: P, z: P)[Q: Specialized](y: Q, f: P) = 
        val b = new B[P](x) {}
        val c = new B[Q](y) {}
        c

@main def Test = 
    val a = foo[String]("Good evening")[Long]("Good morning", 1_000_000_000_000_000L, 1_000_000_000_000_001L)
    assert(a.getArgs == ("Good evening", "Good morning", 1_000_000_000_000_000L, 1_000_000_000_000_001L))

    val b = new B[Boolean](true) {}
    assert(b.foo[Float](1.1, 2.2)[Short](1, 3.3).w == 1)
