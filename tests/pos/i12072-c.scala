object T {

  transparent inline def f(inline s: String): String | Null =
    null

  inline val V = "V"
  inline def D = "D"

  trait Trait { def s: String }

  // ===========================================================================
  // inline {if,match} over inline {val,def}

  transparent inline def if_v: String =
    inline if V == "V" then "o" else "x"

  transparent inline def if_d: String =
    inline if D == "D" then "o" else "x"

  transparent inline def match_v: String =
    inline V match { case "V" => "o"; case _   => "x" }

  transparent inline def match_d: String =
    inline D match { case "D" => "o"; case _   => "x" }

  // ===========================================================================
  // inline {if,match} over inline f(inline {val,def})

  transparent inline def if_fv: String =
    inline if f(V) == "V" then "o" else "x"

  transparent inline def if_fd: String =
    inline if f(D) == "D" then "o" else "x"

  transparent inline def match_fv: String =
    inline f(V) match { case "V" => "o"; case _   => "x" }

  transparent inline def match_fd: String =
    inline f(D) match { case "D" => "o"; case _   => "x" }

  // ===========================================================================
  // inline {if,match} over inline {val,def} in overridden method

  object IfV extends Trait {
    override transparent inline def s: String =
      inline if V == "V" then "o" else "x"
  }

  object IfD extends Trait {
    override transparent inline def s: String =
      inline if D == "D" then "o" else "x" // <--------------------------- error
  }

  object MatchV extends Trait {
    override transparent inline def s: String =
      inline V match { case "V" => "o"; case _   => "x" }
  }

  object MatchD extends Trait {
    override transparent inline def s: String =
      inline D match { case "D" => "o"; case _   => "x" }
  }

  // ===========================================================================
  // inline {if,match} over inline f(inline {val,def}) in overridden method

  object IfFV extends Trait {
    override transparent inline def s: String =
      inline if f(V) == "V" then "o" else "x" // <------------------------ error
  }

  object IfFD extends Trait {
    override transparent inline def s: String =
      inline if f(D) == "D" then "o" else "x" // <------------------------ error
  }

  object MatchFV extends Trait {
    override transparent inline def s: String =
      inline f(V) match { case "V" => "o"; case _   => "x" }
  }

  object MatchFD extends Trait {
    override transparent inline def s: String =
      inline f(D) match { case "D" => "o"; case _   => "x" }
  }
}
