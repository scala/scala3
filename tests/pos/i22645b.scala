import language.experimental.namedTuples
object ExhibitE:  // works

  type N = ("name", "age")
  type Names = Tuple.Map[N, [X] =>> X]

  class SelectableNT extends Selectable:
    def selectDynamic(name: String) = ???
    type Fields = NamedTuple.NamedTuple[Names, (String, Int)]

  val x = new SelectableNT
  x.name