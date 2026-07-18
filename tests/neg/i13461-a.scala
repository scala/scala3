package i13461:

  opaque type Opaque = Int
  transparent inline def op: Opaque = (123: Opaque)

  object Main:
    def main(args: Array[String]): Unit =
      val o22: 123 = op // error

