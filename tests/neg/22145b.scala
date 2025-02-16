package foo

import language.experimental.modularity

trait Collection:
  me =>

  type Self
  type Position
  type Element

  final class Slice(private[Collection] val base: Self, val start: Position, val end: Position):

    final def apply(p: Position): Element =
      require(base.isWithin(p, start, end), "position is out of bounds") // error
      base.apply(p)

  end Slice

  given Slice is Collection:

    type Position = me.Position
    type Element = me.Element

    extension (self: Self)
      def start: Position = self.start
      def end: Position = self.end
      def positionAfter(p: Position): Position = self.base.positionAfter(p) // error
      def apply(p: Position): Element = self.base.apply(p) // error

  end given

  extension (self: Self)

    def start: Position
    def end: Position
    def positionAfter(p: Position): Position
    def apply(p: Position): Element

  end extension
