case class Address(streetNumber: Int, streetName: String)
case class Employee(name: String, addr: Address)

sealed trait Json
case object JNull extends Json
case class JStr(v: String) extends Json
case class JNum(v: Double) extends Json
case class JObj(v: Map[String, Json]) extends Json

object Test {
  def main(args: Array[String]): Unit = {
    val len = GenLens[Address](_.streetNumber)
    val address = Address(10, "High Street")
    assert(len.get(address) == 10)
    val addr2 = len.set(5, address)
    assert(len.get(addr2) == 5)

    // a.b.c
    val len2 = GenLens[Employee](_.addr.streetNumber)
    val employee = Employee("Bob", Address(10, "High Street"))
    assert(len2.get(employee) == 10)
    val employee2 = len2.set(5, employee)
    assert(employee2.name == "Bob")
    assert(len2.get(employee2) == 5)

    // prism
    val jStr: Prism[Json, JStr] = GenPrism[Json, JStr]
    assert(jStr.getOption(JNum(4.5)) == None)
    assert(jStr.getOption(JStr("hello")) == Some(JStr("hello")))
    assert(jStr(JStr("world")) == JStr("world"))

    assert(GenIso[JStr, String].to(JStr("Hello")) == "Hello")
    assert(GenIso.unit[JNull.type].to(JNull) == 1)
    assert(GenIso.unit[JNull.type].from(1) == JNull)

    // TODO: require whitebox macros
    // assert(GenIso.fields[Address].from((0, "a")) == Address(0, "a"))

    val jNum: Prism[Json, Double] = GenPrism[Json, JNum] composeIso GenIso[JNum, Double]
    assert(jNum(3.5) == JNum(3.5))
    assert(jNum.getOption(JNum(3.5)) == Some(3.5))
    assert(jNum.getOption(JNull) == None)

    // inner classes
    val inner = new Inner
    assert(GenIso[inner.JStr, String].to(inner.JStr("Hello")) == "Hello")
    assert(GenIso.unit[inner.JNull.type].to(inner.JNull) == 1)
    assert(GenIso.unit[inner.JNull.type].from(1) == inner.JNull)
  }
}

class Inner {
  sealed trait Json
  case object JNull extends Json
  case class JStr(v: String) extends Json
  case class JNum(v: Double) extends Json
  case class JObj(v: Map[String, Json]) extends Json
}