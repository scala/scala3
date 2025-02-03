trait P:
  def foo: Int

class A extends P:
  export this.foo // error