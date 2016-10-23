object Foobar {
  def apply(): Option[String] = { 
    def foobar[A](f: (String, String) => A): List[A] = List[String]() match { 
      case _ :: ls => foobar(f) 
    } 
    foobar((a1, a2) => (a1 + a2)).headOption 
  } 
}

