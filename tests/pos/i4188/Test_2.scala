package collections

import decorators._

object Test {
  def test(map: Map[Int, String]) = {
    MapDecorator(map.view)
  }
}