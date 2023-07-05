class Conf

class Bar(_conf: Conf) {
  implicit val conf: Conf = _conf
}

class Foo(conf: Conf) extends Bar(conf)
//class Foo(_conf: Conf) extends Bar(_conf)
// using a different name fixes it

class Test {
  def test(foo: Foo) = {
    import foo.*
    //implicit val conf: Conf = foo.conf
    // manually redefining it also fixes it
    assert(conf != null)
    assert(implicitly[Conf] != null)
  }
  def test2(foo: Foo) = {
    import foo.conf
    //implicit val conf: Conf = foo.conf
    // manually redefining it also fixes it
    assert(conf != null)
    assert(implicitly[Conf] != null)
  }
}