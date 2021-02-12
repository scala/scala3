object Test {
  // crash
  def foo(a: Any) = { import a.toString as toS; toS }

  // okay
  def ok1(a: String) = { import a.isInstanceOf as iio; iio[String] }
  def ok2(a: Int) = { import a.toInt as ti; ti }
}
