case class Address(streetNumber: Int, streetName: String)

case class Employee(name: String, addr: Address)

object Test {
  def main(args: Array[String]): Unit = {
    val len = GenLens[Address](_.streetNumber)
    val address = Address(10, "High Street")
    assert(len.get(address) == 10)
    val addr2 = len.set(5, address)
    assert(len.get(addr2) == 5)

    val len2 = GenLens[Employee](_.addr.streetNumber)
    val employee = Employee("Bob", Address(10, "High Street"))
    assert(len2.get(employee) == 10)
    val employee2 = len2.set(5, employee)
    assert(employee2.name == "Bob")
    assert(len2.get(employee2) == 5)
  }
}