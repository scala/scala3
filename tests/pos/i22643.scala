import language.experimental.namedTuples



object ExhibitB:

  trait JoinB[A <: Tuple, B <: Tuple]:
    type NTB = NamedTuple.NamedTuple[Tuple.Concat[A, B], (String, Int)]
    val ntB: NTB = ???

  val joinB: JoinB[Tuple1["nameB"], Tuple1["ageB"]] = ???

  joinB.ntB.nameB    // works


object ExhibitC:

  type A = Tuple1["nameC"]
  type B = Tuple1["ageC"]

  type NamesC = Tuple.Concat[A, B]
  type NTC = NamedTuple.NamedTuple[NamesC, (String, Int)]
  val ntC: NTC = ???

  ntC.nameC   // works


object ExhibitD:

  trait JoinD[A, B]:
    type NamesD = (A, B)
    type NTD = NamedTuple.NamedTuple[NamesD, (String, Int)]
    val ntD: NTD = ???

  val joinD: JoinD["nameD", "ageD"] = ???

  joinD.ntD.nameD    // works

object ExhibitA:

  trait JoinA[A <: Tuple, B <: Tuple]:
    type NamesA = Tuple.Concat[A, B]
    type NTA = NamedTuple.NamedTuple[NamesA, (String, Int)]
    val ntA: NTA = ???

  val joinA: JoinA[Tuple1["nameA"], Tuple1["ageA"]] = ???

  joinA.ntA.nameA    // fixed
