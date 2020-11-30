
@main def Test = {
  val personA: PersonA = PersonA("JoeA")
  val personB: PersonB = PersonB("JoeB")
  val person: Person = personA
  MatchMac(personA.name)
  MatchMac(personB.name)
  MatchMac(person.name)
  MatchMac(PersonA("JoeA").name) // optimized to MatchMac("JoeA")
  MatchMac(PersonB("JoeB").name) // optimized to MatchMac("JoeB")
}
