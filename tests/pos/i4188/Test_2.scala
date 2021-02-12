package collections

import decorators.*

object Test {
  def test(map: Map[Int, String]) = {
    MapDecorator(map.view)
  }
}