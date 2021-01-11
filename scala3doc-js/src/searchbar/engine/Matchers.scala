package dotty.dokka

enum Matchers(func: (PageEntry) => Int) extends Function1[PageEntry, Int]:
  export func.apply
  case ByName(query: String) extends Matchers( (p) => {
    val nameOption = Option(p.shortName)
    val acronym = p.acronym
    //Edge case for empty query string
    if query == "" then 1
    else {
      val results = List(
        nameOption.filter(_.contains(query.toLowerCase)).fold(-1)(_.size - query.size),
        acronym.filter(_.contains(query)).fold(-1)(_.size - query.size + 1)
      )
      if results.forall(_ == -1) then -1 else results.filter(_ != -1).min
    }
  })
  case ByKind(kind: String) extends Matchers((p) => p.fullName.split(" ").headOption.filter(_.equalsIgnoreCase(kind)).fold(-1)(_ => 1))

