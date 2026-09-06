//> using options -language:experimental.inlineTraits
class A:
    def foo: Int = 12

inline trait B(x: Int) extends A:
    val y = x
    override def foo = 10 

class C extends B(15)
