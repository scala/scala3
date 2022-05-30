class A:
  private var x = 0
  inline def f1(): Int =
    x += 1;
    x

class B extends A:
  private var x = 0 // error
  inline def f2(): Int =
    x += 1
    x
