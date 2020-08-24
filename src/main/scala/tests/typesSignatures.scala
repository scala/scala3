package tests
package typesSignatures

class A
{
  type A = Int
  // type B[+T] = Seq[T] It's translated into:
  type B = [T] =>> Seq[T]
  //variance is checked by compiler and info about it has no use for developer 
  
  //type C[A, B <: A] = Seq[B] Equivalent form:
  type C = [A, B <: A] =>> Seq[B]
}

trait V 
{
  type Ala[+J] <: Int
  type Ola[+T]
  type X
} 

class Generic[T]

class Base
{
  type A
  type B = Int

  // Tests not support multiline signatures
  // type MatchT[T] = T match { case String => Char case Int => Byte } Equivalent form:
  type MatchT = [T] =>> T match { case String => Char case Int => Byte }

  // Tests do not support multiline signatures
  // type Elem[X] = X match { case String => Char case Array[t] => t case Iterable[t] => t } Equivalent form:
  type Elem = [X] =>> X match { case String => Char case Array[t] => t case Iterable[t] => t }
}