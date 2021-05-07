object T2 {
  import M.f

  private inline val V = "V"
  private inline def D = "D"

  trait Trait { def s: String }

  object MatchFV extends Trait {
    override transparent inline def s: String =
      inline f(V) match { case "V" => "o"; case _   => "x" } // error in RC1
  }

  object MatchFD extends Trait {
    override transparent inline def s: String =
      inline f(D) match { case "D" => "o"; case _   => "x" }
  }
}
