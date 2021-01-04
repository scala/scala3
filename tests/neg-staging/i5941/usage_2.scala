case class Address(streetNumber: Int, streetName: String)

object Test {
  def main(args: Array[String]): Unit = {
    val len = GenLens[Address](_.streetNumber + 3) // error
    val address = Address(10, "High Street")
    assert(len.get(address) == 10)
    val addr2 = len.set(5, address)
    assert(len.get(addr2) == 5)
  }
}