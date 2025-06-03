import language.experimental.namedTuples
object ExhibitA:  //  fails

  class SelectableNT[N <: Tuple] extends Selectable:
    def selectDynamic(name: String) = ???
    type Names = Tuple.Map[N, [X] =>> X]
    type Fields = NamedTuple.NamedTuple[Names, (String, Int)]

  val x = new SelectableNT[("name", "age")]
  x.name  // fails


object ExhibitB:  //  works

  class SelectableNT[N <: Tuple] extends Selectable:
    def selectDynamic(name: String) = ???
    type Fields = NamedTuple.NamedTuple[N, (String, Int)]

  val x = new SelectableNT[("name", "age")]
  x.name


object ExhibitC:  // works

  class SelectableNT[N <: Tuple] extends Selectable:
    def selectDynamic(name: String) = ???
    type Fields = NamedTuple.NamedTuple[N, (String, Int)]

  type N = ("name", "age")
  val x = new SelectableNT[N]
  x.name


object ExhibitD:  // works

  class SelectableNT[N <: Tuple] extends Selectable:
    def selectDynamic(name: String) = ???
    type Fields = NamedTuple.NamedTuple[N, (String, Int)]

  type N = ("name", "age")
  type Names = Tuple.Map[N, [X] =>> X]
  val x = new SelectableNT[Names]
  x.name

