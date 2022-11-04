class StringOps:
  extension (x: String)
    def capitalize: String = ???
    def foo: String = ???

  def foo: String = ???


extension (s: String)
  private def moreOps = new StringOps()
  export moreOps.capitalize // error: no eligible member capitalize at moreOps

extension (s: String)
  private def moreOps2= new StringOps()
  export moreOps2.foo // OK

