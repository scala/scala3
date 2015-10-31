object Test {
  def test = {
    val myName: String = ""
    new AnyRef {
      new Exception {
        def name = myName
      }
    }
    new AnyRef {
      new Exception {
        new AnyRef {
          def name = myName
        }
      }
    }
  }
}
