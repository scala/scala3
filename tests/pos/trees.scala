class Tree[T]

class Empty[T] extends Tree[Nothing]
class Node[T](elem: T, l: Tree[T], r: Tree[T])

