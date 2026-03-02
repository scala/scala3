sealed trait NodeId
case object Hi extends NodeId
case object Hello extends NodeId

extension (value: NodeId)
  inline def parse[T <: NodeId] = value match
    case _: T => ()
    case _    => ()

object Test:
  def main(args: Array[String]): Unit =
    Hi.parse[Hello.type]

