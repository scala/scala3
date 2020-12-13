package tests
package inheritanceLoop

class A
{
  type I = Int
  object X
  class B extends C
  {
    class D extends C
    {
      class E extends C
    }
  }
}


class C extends A
