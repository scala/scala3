//> using options -language:experimental.specializedTraits

inline trait T1[T]:
    def boo(x: T): T = x

inline trait T[T: Specialized]:
    def ff = new T1[T]() {
        def id(x: T): T = x  
    }

@main def Test =
    val a = new T[Int]() {}
