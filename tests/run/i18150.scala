object Test: 
  def main(args: Array[String]): Unit =
    val result = 
      scala.compiletime.testing.typeCheckErrors(
        "trait RecursiveSelfTypeEntity[E <: RecursiveSelfTypeEntity[E]]: \n" +
        "  self: E => \n" +
        "  def create(): E \n" +
        "  def read(id: Long): Option[E] \n" +
        "  def update(f: E => E): E \n" +
        "  def delete(id: Long): Unit \n" +
        "\n" +
        "class Apple extends RecursiveSelfTypeEntity[Apple]: \n" +
        "  override def create(): Apple = ??? \n" +
        "  override def read(id: Long): Option[Apple] = ??? \n" +
        "  override def update(f: Apple => Apple): Apple = ??? \n" +
        "  override def delete(id: Long): Unit = ??? \n" +
        " \n" +
        "class Orange extends RecursiveSelfTypeEntity[Orange]: \n" +
        "  override def create(): Orange = ??? \n" +
        "  override def read(id: Long): Option[Orange] = ??? \n" +
        "  override def update(f: Orange => Orange): Orange = ??? \n" +
        "  override def delete(id: Long): Unit = ??? \n" + 
        " \n" +
        "class Banana extends RecursiveSelfTypeEntity[Apple]: \n" +
        "  override def create(): Apple = ??? \n" +
        "  override def read(id: Long): Option[Apple] = ??? \n" +
        "  override def update(f: Apple => Apple): Apple = ??? \n" +
        "  override def delete(id: Long): Unit = ???\n"
      )
    assert(!result.isEmpty, "Should fail type check, but it didn't.")
    println(result)
