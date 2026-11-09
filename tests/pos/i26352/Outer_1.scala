// https://github.com/scala/scala3/issues/26352
class Box[I]

abstract class Outer:
  self =>
  object inner:
    inline def make: Box[?] = new Box[self.type]
