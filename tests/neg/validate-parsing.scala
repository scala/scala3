object A {
  sealed def y: Int = 1 // error: modifier(s) `sealed' not allowed for method
  sealed var x = 1 // error: modifier(s) `sealed' not allowed for variable
  lazy trait T // error: modifier(s) `lazy' not allowed for trait
}

class C () {
  implicit this() = this() // error: ';' expected but 'implicit' found.
  override this() = this() // error: ';' expected but 'override' found.
}
class D override() // error: ';' expected but 'override' found.

