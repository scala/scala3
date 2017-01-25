package dotty

import org.junit.Test
import org.junit.Assert._

class ShowTests {
  import Show._

  @Test def showString = {
    assertEquals("\"\\thello world!\"", "\thello world!".show)
    assertEquals("\"\\nhello world!\"", "\nhello world!".show)
    assertEquals("\"\\rhello world!\"", "\rhello world!".show)
    assertEquals("\"\\b\\t\\n\\f\\r\\\'\\\"\"", "\b\t\n\f\r\'\"".show)
  }

  @Test def showFloat = {
    assertEquals("1.0f", 1.0f.show)
    assertEquals("1.0f", 1.0F.show)
  }

  @Test def showDouble = {
    assertEquals("1.0", 1.0d.show)
    assertEquals("1.0", 1.0.show)
  }

  @Test def showChar = {
    assertEquals("'\\b'", '\b'.show)
    assertEquals("'\\t'", '\t'.show)
    assertEquals("'\\n'", '\n'.show)
    assertEquals("'\\f'", '\f'.show)
    assertEquals("'\\r'", '\r'.show)
    assertEquals("'\\''", '\''.show)
    assertEquals("'\\\"'", '\"'.show)
  }

  @Test def showCar = {
    case class Car(model: String, manufacturer: String, year: Int)
    implicit val showCar = new Show[Car] {
      def show(c: Car) =
        "Car(" + c.model.show + ", " + c.manufacturer.show + ", " + c.year.show + ")"
    }

    case class Shop(xs: List[Car], name: String)
    implicit val showShop = new Show[Shop] {
      def show(sh: Shop) =
        "Shop(" + sh.xs.show + ", " + sh.name.show + ")"
    }

    assertEquals("Car(\"Mustang\", \"Ford\", 1967)", Car("Mustang", "Ford", 1967).show)
  }

  @Test def showOptions = {
    assertEquals("None", None.show)
    assertEquals("None", (None: Option[String]).show)
    assertEquals("Some(\"hello opt\")", Some("hello opt").show)
  }

  @Test def showMaps = {
    val mp = scala.collection.immutable.Map("str1" -> "val1", "str2" -> "val2")
    assertEquals("Map(\"str1\" -> \"val1\", \"str2\" -> \"val2\")", mp.show)
  }

  @Test def withoutShow = {
    case class Car(model: String, manufacturer: String, year: Int)

    assertEquals("Car(Mustang,Ford,1967)", Car("Mustang", "Ford", 1967).show)
    assertEquals("Map()", Map().show)
  }
}
