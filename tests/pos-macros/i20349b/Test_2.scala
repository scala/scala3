class Test {
  object domain {
    enum PaymentMethod:
      case PayPal(email: String)
      case Card(digits: Long, name: String)
      case Cash
  }
  println(Macros.values[domain.PaymentMethod])
}
object Test {
  lazy val script = new Test()
  def main(args: Array[String]): Unit =
    val _ = script.hashCode()
}
