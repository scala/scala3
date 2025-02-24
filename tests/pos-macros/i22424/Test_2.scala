@main def Test =
  val m = new PolymorphicTrait {}
  MockMaker.inlineMock[m.Embedded]
  MockMaker.transparentInlineMock[m.Embedded]
