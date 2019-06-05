import annotation.alpha
class C extends B_1 {   // error: Name clash between defined and inherited member
  @alpha("bar") def foo(): Int = 3
}
