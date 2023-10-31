trait SQLSyntaxSupport[A]

trait ResultNameSQLSyntaxProvider[S <: SQLSyntaxSupport[A], A]
trait QuerySQLSyntaxProvider[S <: SQLSyntaxSupport[A], A]{
  def resultName: ResultNameSQLSyntaxProvider[S, A] = ???
}

def include(syntaxProviders: QuerySQLSyntaxProvider[?, ?]*) = {
  syntax(syntaxProviders.map(_.resultName): _*)
}

def syntax(resultNames: ResultNameSQLSyntaxProvider[?, ?]*) = ???