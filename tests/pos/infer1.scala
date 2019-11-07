trait TestInferrence[T] {

  def getInt(t: T): Int

}

object TestIntInferrence extends TestInferrence[Int] {
  override def getInt(i: Int) = i
}

object InferrenceTest {

  def createNumberHandler[T](
    testInfer: TestInferrence[T] = TestIntInferrence,
    handlers: Map[String, T => Unit] = Map.empty,
  ): T => Unit = {

    (t: T) => {
      testInfer.getInt(t)
      ()
    }

  }
}

class InferrenceTest {

  val handler = InferrenceTest.createNumberHandler()

}
