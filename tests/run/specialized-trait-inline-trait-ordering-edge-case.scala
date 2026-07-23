// scalajs: --skip
//> using options -language:experimental.specializedTraits

inline trait T1[A: Specialized]

inline trait T2[B: Specialized]:
    def foo(x: T1[B]) = "bungle"

class Bar extends T2[Int] // Inlined by specializeInlineTraits
                          // DesugarSpecializedTraits then needs to see the inlined T1[Int] and generate the specialization
                          // This works ok right now because of the miniphase processing but it's perhaps not ideal.         
@main def Test = 
    val fooMethods = classOf[Bar].getDeclaredMethods.iterator
        .filter(_.getName.startsWith("foo"))
        .map(m => m.getName -> m.getParameterTypes.head.getName)
        .toList
    assert(fooMethods.exists(_._2 == "T1$$sp$scala$Int"))
