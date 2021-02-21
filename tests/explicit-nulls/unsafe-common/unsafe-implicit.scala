class S {

  def f1(using String) = {}

  def f2(using String | Null) = {}

  locally {
    implicit val x: String = ???

    val y1: String = summon
    val y2: String | Null = summon
  }

  def test1(implicit x: String) = {
    val y1: String = summon
    val y2: String | Null = summon
  }

  def test2(using String) = {
    val y1: String = summon
    val y2: String | Null = summon
  }

  def test3(using String) = {
    f1
    f2
  }

  locally {
    implicit val x: String | Null = ???

    val y1: String = summon // error
    val y2: String | Null = summon
  }

  def test4(implicit x: String | Null) = {
    val y1: String = summon // error
    val y2: String | Null = summon
  }

  def test5(using String | Null) = {
    val y1: String = summon // error
    val y2: String | Null = summon
  }

  def test6(using String | Null) = {
    f1 // error
    f2
  }

  locally {
    // OfType Implicits

    import java.nio.charset.StandardCharsets
    import scala.io.Codec

    val c: Codec = StandardCharsets.UTF_8 // error
  }
}