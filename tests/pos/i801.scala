object T1 {
  import java.util.ArrayList, java.util.stream.Stream as JStream
  new java.util.ArrayList[String]().stream.map(_.toInt).map(_.toString): JStream[String]
}

object T2 {
  import java.util._, java.util.stream.Stream as JStream
  def f: JStream[String] = new java.util.ArrayList[String](Arrays.asList("1", "2")).stream.map(_.toInt).map(_.toString)
}
