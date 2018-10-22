trait Foo {
  def getName: String

  def getTitle: String

  val message = "hello, " + getTitle + " " + getName
}

class Bar(val name: String) extends Foo {
  val title = "Mr."

  def getName = name                 // ok: name is a Param field

  def getTitle = title               // error: cannot use title // error
}

object Test {
  def main(args: Array[String]): Unit = {
    new Bar("Jack")
  }
}

trait Dao(val name: String) extends Foo {
  val title = "Mr."

  def getName = name
}

trait Zen(val name: String) {
  val title = "Mr."

  @scala.annotation.partial
  def getName = name          // error: cannot access `name`  // error
}