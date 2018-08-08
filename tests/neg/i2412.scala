object Test {
  def test(foo: List[String]): Unit = {
    foo.filter(f => {
      iDontExist // error
      true
    })
  }
}
