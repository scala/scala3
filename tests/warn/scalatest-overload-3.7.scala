import scala.language.`3.7`

class TestBody1
class TestBody2

class StartWithWord
class EndWithWord

class Matchers:
  extension (leftSideString: String)
    def should(body: TestBody1): Unit = ()
    def should(body: TestBody2): Unit = ()

  extension [T](leftSideValue: T)
    def should(word: StartWithWord)(using T <:< String): Unit = ()
    def should(word: EndWithWord)(using T <:< String): Unit = ()

  def endWith(rightSideString: String): EndWithWord = new EndWithWord

class Test extends Matchers:
  def test(): Unit =
    "hello world" should endWith ("world") // warn: overloading resolution change
