import annotation.targetName
class C extends B_1 {   // error: Name clash between defined and inherited member
  @targetName("bar") def foo(): Int = 3
}
