type Head[X] = X match {
  case Tuple2[a, b] => a
}

trait Z {
  type Y
  def unpair[X <: Y]: Head[X]
}

class A extends Z {
  type Y <: Tuple2[Any, Any]
  def unpair[X <: Y]: Head[X] = ""
  def any[X <: Y]: Any = unpair[X]
}

class B extends A { this: Z =>
  type Y = Tuple2[Int, Int]
  def int[X <: Y]: Int = unpair[X]  // error
}

class C extends A { this: Z =>
  type Y = Tuple2[String, String]
  def string[X <: Y]: String = unpair[X]   // error
}

object Main {
  def main(args: Array[String]): Unit = {
    println((new B).int + 1) // would give ClassCastException
  }
}