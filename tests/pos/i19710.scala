import scala.util.NotGiven

type HasName1 = [n] =>> [x] =>> x match {
    case n => true
    case _ => false
  }
@main def Test = {
  summon[HasName1["foo"]["foo"] =:= true]
  summon[NotGiven[HasName1["foo"]["bar"] =:= true]]
  summon[Tuple.Filter[(1, "foo", 2, "bar"), HasName1["foo"]] =:= Tuple1["foo"]] // error
}
