//> using options -language:experimental.inlineTraits
inline trait A(val x: Int = 4)

class B extends A(1)
class C extends A()