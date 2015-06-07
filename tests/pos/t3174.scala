object test {
  def method(): Unit = {
    class Foo extends AnyRef {
      object Color {
        object Blue {
          //val b = new Board
        }
      }

      class Board {
        val grid = Color.Blue
      }
    }
    new Foo
  }
 }
