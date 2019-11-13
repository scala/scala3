import scala.reflect.TypeTest

object Test {
  def test[S, T](using x: TypeTest[S, T]): Unit = ()

  test[Any, AnyRef] // error
  test[Any, AnyVal] // error
  test[Any, Object] // error

  test[Any, Nothing] // error
  test[AnyRef, Nothing] // error
  test[AnyVal, Nothing] // error
  test[Null, Nothing] // error
  test[Unit, Nothing] // error
  test[Int, Nothing] // error
  test[8, Nothing] // error
  test[List[_], Nothing] // error
  test[Nothing, Nothing] // error
}
