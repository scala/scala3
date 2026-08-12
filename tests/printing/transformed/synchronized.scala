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

  inline def isInline: String = synchronized {
    ""
  }

  def hasOtherCode: String =
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

  def inModule: String = synchronized {
    ""
  }
}

trait T {
  def inTrait: String = synchronized {
    ""
  }
}

class X(val s: String) extends AnyVal {
  def extension1: String = synchronized { "" }
}

extension (s: String) {
  def extension2: String = synchronized { "" }
}

def topLevel: String = synchronized { "" }
