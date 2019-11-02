package adhoc
class B extends A  // error: adhoc-extension (under -strict -feature -Xfatal-warnings)
class C extends A  // error

object O {
  val a = new A {}  // error
  object E extends A  // error
}