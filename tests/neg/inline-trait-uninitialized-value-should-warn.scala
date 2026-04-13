//> using options -Werror -Wsafe-init

inline trait A(val x: Int):
  val f = z
  val z: Int // nopos-error: This should warn with -Wsafe-init
class C extends A(10):
  val z = 10
