object A1 {
  private[this] class B

  private[this] class C {
    def cons: B = new B
  }
}

class A2 {
  private[this] class B

  private[this] class C {
    def cons: B = new B
  }
}
