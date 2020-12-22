package dotty.dokka

enum Matchers(func: (PageEntry) => Int) extends Function1[PageEntry, Int]:
  export func.apply
  case ByName(query: String) extends Matchers( (p) => {
    val name = p.searchKeys.headOption.map(_.toLowerCase)
    name.filter(_.contains(query)).map(_ => p.name.size - query.size).getOrElse(-1)
  })
  case ByKind(kind: String) extends Matchers((p) => p.name.split(" ").headOption.filter(_ == kind).fold(-1)(_ => 1))

