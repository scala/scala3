//> using options -language:experimental.specializedTraits

inline trait Pair[T: Specialized, U: Specialized](x: T, y: U)

object Pair:
  inline def apply[T: Specialized, U](x: T, y: U): Pair[T, U] = new Pair[T, U](x, y) {} 
  inline def apply[T, U: Specialized](x: T, y: U): Pair[T, U] = new Pair[T, U](x, y) {} // error: conflicting definition
  
  def main =
    Pair("Hello World", 42) // error: ambiguous overload