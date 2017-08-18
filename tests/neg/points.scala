class Point extends Comparable[Point] {
  override def compareTo(other: Point): Int = ???
}

class ColoredPoint extends Point with Comparable[ColoredPoint] {
  override def compareTo(other: ColoredPoint): Int = ??? // error: overridden method has different signature
}

