trait A {
  def extension_n: String = "illegal method" // error: illegal name: extension_n may not start with `extension_`
  type extension_type = Int // allowed because it's a type alias
  val extension_val = 23 // error: illegal name: extension_val may not start with `extension_`
  private var extension = Nil // error: not allowed because it matches `extension`
}

extension (x: Any) def extension_foo: String = "foo" // error: illegal name: extension_foo may not start with `extension_`
extension (some: Any) def valid_extension_name: String = "bar"