class Foo {
  implicit class NN[T](x: T|Null) {
    def nn: T = x.asInstanceOf[T]
  }
  import java.util.ArrayList, java.util.stream.{Stream => JStream}
  // Test that we can infer the argument of the lambda in the application of `map`, even
  // though the prototype is of the form Function[A,B]|Null
  new java.util.ArrayList[String]().stream.map(_.nn.toInt).map(_.nn.toString): JStream[String|Null]|Null

  val x: (Int => Int)|Null = (y) => y
}
