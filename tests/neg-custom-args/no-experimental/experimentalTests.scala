import scala.annotation.experimental

@experimental def x = 2

class MyTests {
  /*@Test*/ def test1 = x // error
  @experimental
  /*@Test*/ def test2 = x
}

@experimental
class MyExperimentalTests {
  /*@Test*/ def test1 = x
  /*@Test*/ def test2 = x
}
