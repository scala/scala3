trait Node
type NodeParser[T] = Node => T

def child(key: String): Option[Node] = ???

def optionalOneOf[T](in: Map[String, NodeParser[? <: T]]): Option[T] =
  val mappings = in flatMap { (key, parser) =>
    child(key) map { node =>
      key -> (() => parser(node))
    }
  }
  mappings.headOption map (_._2())
