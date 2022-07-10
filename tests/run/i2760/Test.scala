// scalajs: --skip

@Fork(value = 16) class HasFork

object Test {
  def main(args: Array[String]): Unit = {
    val fork = classOf[HasFork].getAnnotation(classOf[Fork])
    assert(fork.value == 16, s"fork.value is ${fork.value} but should have been 16")
    assert(fork.warmups == -1, s"fork.warmups is ${fork.warmups} but should have been -1")
  }
}
