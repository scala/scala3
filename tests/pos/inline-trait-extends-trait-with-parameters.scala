//> using options -language:experimental.inlineTraits
trait A(val z: Int):
    def foo: Int

inline trait B(x: Int) extends A:
    val y = x + z
    override def foo = 10 

class C extends B(15), A(10)
