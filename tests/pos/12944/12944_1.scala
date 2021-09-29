object Test1 {
  type ++[L, R] = (L, R) match
    case (Int, Int)       => 2
    case (String, String) => "2"
    case (String, Int)    => "2"
    case (Int, String)    => "2"

  type Bar[W <: Int] = W ++ "" ++ W

  val works = summon[Bar[2] =:= "2"]
}
