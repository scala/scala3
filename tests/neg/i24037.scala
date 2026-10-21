// Test for issue #24037: Illegal applyDynamic signature should give error, not StackOverflowError
// https://github.com/scala/scala3/issues/24037

// Case 1: Original issue - illegal applyDynamic signature (only one parameter list)
object Test1:
  val s = new Selectable:
    type Fields = (f: Int)
    def selectDynamic(f: String): Any = 1
    def applyDynamic(f: String): Any = 1  // illegal: missing second parameter list for args

  val x = s.f  // error

// Case 2: Another variant - missing varargs parameter
object Test2:
  val s = new Selectable:
    type Fields = (m: (x: Int) => Int)
    def selectDynamic(name: String): Any = ???
    def applyDynamic(name: String): Any = ???  // illegal: should take (String)(Any*) or similar

  val y = s.m(42)  // error

// Case 3: Wrong return type but should still not crash
object Test3:
  val s = new Selectable:
    type Fields = (g: Int)
    def selectDynamic(name: String): String = "wrong"  // returns String not Any

  val z = s.g  // error: type mismatch
