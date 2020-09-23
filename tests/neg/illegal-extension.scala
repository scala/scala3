trait A {
  def extension_n: String = "illegal method" // error: illegal method name: extension_n may not start with `extension_`
}

extension (x: Any) def extension_foo: String = "foo" // error: illegal method name: extension_foo may not start with `extension_`
