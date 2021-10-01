object Test:
  inline def jim(seq: Seq[Int]) =
    inline seq match
      case s: Seq[Int] if s.isEmpty => println("seq is empty")
      case s: Seq[Int] => println("seq is not empty")
      case _ => println("somthing hinky happened")

  jim(Seq(1,2)) // error
