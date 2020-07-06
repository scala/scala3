trait A {
  def extension_n: String = "illegal method" // error: illegal method name: extension_n may not start with `extension_`
}
