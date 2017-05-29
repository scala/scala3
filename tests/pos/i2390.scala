trait TestSuite {
  trait NoArgTest
}

trait TestSuiteMixin { self: TestSuite =>
  def foo(test: self.NoArgTest) = {}
}

trait CancelAfterFailure extends TestSuiteMixin { self: TestSuite =>
  override def foo(test: self.NoArgTest) = {}
}
