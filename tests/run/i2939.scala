import scala.collection.mutable.*

class Tag(val name: String, val buffer: Buffer[Tag] = ArrayBuffer()) {
	def space(n: Int = 0): String = {
	    s"${" " * n}<$name>\n" +
	        (if(buffer.isEmpty) "" else buffer.map(_.space(n + 4)).mkString("", "\n", "\n")) +
	    s"${" " * n}</$name>"
	}

	def apply[U](f: Tag ?=> U)(implicit tag: Tag = null): this.type = {
		f(using this)
		if(tag != null) tag.buffer += this
		this
	}

	override def toString(): String = space(0)
}

object Tag {
	implicit def toTag(symbol: Symbol): Tag = new Tag(symbol.name)

	def main(args: Array[String]): Unit = {
	}
}


object Test {
  def foo(x: Int => Int)(y: Int = 0) = {}
  def bar(x: => Int)(y: Int = 0) = {}

  def main(args: Array[String]): Unit = {
    foo(x => x)()
    bar(args.length)()
  }
}
