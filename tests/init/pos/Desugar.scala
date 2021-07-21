trait A

class Tree[-A >: Int] {
    val x: Int = 10
}

case class C[-T >: Int] (lhs: Int, rhs: Tree[T]) extends A {
    val x = rhs.x
}

object DesugarError {
    val f: PartialFunction[A, Int] = {case C(_, rhs) => rhs.x}
}


