package tests
package typesSignatures

class A
{
  type A = Int
  type B[+T] = Seq[T]
  type C[A, B <: A] = Seq[B]
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
}