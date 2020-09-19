trait A {
  def extension_n: String = "illegal method" // error: illegal method name: extension_n may not start with `extension_`
  type extension_type = Int
  var extension_val = 23 // error: illegal value name: extension_val may not start with `extension_`
  val extension_private = "good" // allowed because it's immutable
}

extension (x: Any) def extension_foo: String = "foo" // error: illegal method name: extension_foo may not start with `extension_`
class B {
  private var extension_private_var = "good" // allowed because the owner is a class
}
