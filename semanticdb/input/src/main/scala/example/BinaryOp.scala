package example

class BinaryOp {
  val y = 1 #:: 2 #:: Stream.empty[Int]
  val x = 1 :: 2 :: 3 :: Nil
  val z = 1 + 2
}