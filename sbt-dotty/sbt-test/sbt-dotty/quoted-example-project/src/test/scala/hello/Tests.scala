package hello

import org.junit.Test

// Import Expr and some extension methods
import scala.quoted._

class Tests {

  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)

  @Test def test(): Unit = {
    import hello.Main._

    val square = stagedPower(2)

    assert(Math.pow(3, 2) == square(3))

    square(3)
    square(4)

    assert(Math.pow(4, 2) == square(4))

    val cube = stagedPower(3)
    cube(2)


    assert(Math.pow(2, 3) == cube(2))



    val toTheFourth = stagedPower(4)
    assert(Math.pow(3, 4) == toTheFourth(3))
  }

}
