trait A {
  def extension_n: String = "illegal method" // error: illegal name: extension_n may not start with `extension_`
  type extension_type = Int // allowed because it's a type alias
  val extension_val = 23 // error: illegal name: extension_val may not start with `extension_`
  private var extension = Nil // error: illegal setter name: `extension_=`
}

class B {
  var extension = 1337 // error: illegal setter name: `extension_=`
}

class C {
  private var extension = "OK" // allowed because it does not require a setter
}

extension (x: Any) def extension_foo: String = "foo" // error: illegal name: extension_foo may not start with `extension_`
extension (some: Any) def valid_extension_name: String = {
  var extension = "foo" // `extension` name allowed because it doesn't require a setter
  s"$extension bar"
}