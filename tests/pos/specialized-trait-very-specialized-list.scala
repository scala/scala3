//> using options -language:experimental.specializedTraits
inline trait Foo[T: Specialized]

abstract class VerySpecializedList extends Seq[Foo[Int]]

def main = 
    val x: VerySpecializedList = null
    val y: Foo[Int] = x(1)
