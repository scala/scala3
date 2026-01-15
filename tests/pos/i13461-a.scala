package i13461:

  opaque type Opaque = Int
  transparent inline def op: Opaque = 123
  transparent inline def oop: i13461.Opaque = 123

  object Main:
    def main(args: Array[String]): Unit =
      val o2: Opaque = op
      val o3: Opaque = oop // needs to be unwrapped from Typed generated in adapt
      val o22: 123 = op
      val o23: 123 = oop
