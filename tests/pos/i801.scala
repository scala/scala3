class Foo {
  implicit class NN[T](x: T|Null) {
    def nn: T = x.asInstanceOf[T]
  }

  object T1 {
    import java.util.ArrayList, java.util.stream.{Stream => JStream}
    new java.util.ArrayList[String]().stream.map(_.nn.toInt).map(_.nn.toString): JStream[String|Null]|Null
  }

  object T2 {
    import java.util._, java.util.stream.{Stream => JStream}
    def f: JStream[String]|Null = new java.util.ArrayList[String](Arrays.asList[String]("1", "2").nn).stream.map(_.nn.toInt).map(_.nn.toString)
  }
}
