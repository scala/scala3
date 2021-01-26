package outer
package nested

object indent1:
  object inner:
    def x: Int = 1
  end inner
  val y: Int = 2
end indent1
