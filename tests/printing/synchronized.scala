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

  inline def notThisBecauseInline: String = synchronized {
    ""
  }

  def notThisBecauseHasOtherCode: String =
    synchronized {
      ()
    }
    ""
}

object A {
  @scala.annotation.static
  def staticOne: String = synchronized {
    ""
  }
}

trait T {
  def notThisBecauseInATrait: String = synchronized {
    ""
  }
}

class X(val s: String) extends AnyVal {
  def notThisBecauseExtension1: String = synchronized { "" }
}

extension (s: String) {
  def notThisBecauseExtension2: String = synchronized { "" }
}

def notThisBecauseTopLevel: String = synchronized { "" }
