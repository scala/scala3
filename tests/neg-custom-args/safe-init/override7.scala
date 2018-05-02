import scala.annotation.partial

trait Foo {
  @partial
  def getName: String

  @partial
  def getTitle: String

  val message = "hello, " + getTitle + " " + getName
}

class Bar(val name: String) extends Foo {
  val title = "Mr."

  @partial
  def getName = name                 // ok: name is a Param field

  @partial
  def getTitle = title               // error: title cannot use title // error
}

object Test {
  def main(args: Array[String]): Unit = {
    new Bar("Jack")
  }
}

trait Dao(val name: String) extends Foo {
  val title = "Mr."

  @partial
  def getName = name          // error: cannot access `name`  // error
}

trait Zen(val name: String) {
  val title = "Mr."

  @partial
  def getName = name          // error: cannot access `name`  // error
}