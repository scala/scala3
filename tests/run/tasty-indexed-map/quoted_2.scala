object Test {

  def main(args: Array[String]): Unit = {
    val map = MyMap.create[("foo", ("bar", ("baz", Unit)))]

    map.set["foo"](9)
    assert(map.get["foo"] == 9)

    map.set["bar"](42)
    assert(map.get["bar"] == 42)

    map.set["baz"](42)
    assert(map.get["baz"] == 42)

//    map.set["banana"](42) // error
//    assert(map.get["banana"] == 42) // error
  }
}