class A {
  def basic: String = synchronized {
    ""
  }

  def explicit: String =
    this.synchronized {
      ""
    }

  def explicitWithBraces: String = {
    this.synchronized {
      ""
    }
  }

  inline def notThisOneBecauseInline: String = synchronized {
    ""
  }

  def alsoNotThisBecauseHasOtherCode: String =
    synchronized {
      ()
    }
    ""
}
