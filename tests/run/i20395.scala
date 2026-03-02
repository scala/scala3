sealed trait NodeId
case object Hi extends NodeId
case object Hello extends NodeId

extension (value: NodeId)
  inline def parse[T <: NodeId]: Unit = value match
    case _: T => println("match")
    case _    => println("not match")

object Test:
  def main(args: Array[String]): Unit =
    Hi.parse[Hello.type]

