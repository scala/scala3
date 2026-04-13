abstract class A:
    def foo: Int

inline trait B(x: Int) extends A:
    val y = x
    override def foo = 10 

class C extends B(15)
