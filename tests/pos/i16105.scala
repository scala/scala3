trait SQLSyntaxSupport[A]

trait ResultNameSQLSyntaxProvider[S <: SQLSyntaxSupport[A], A]
trait QuerySQLSyntaxProvider[S <: SQLSyntaxSupport[A], A]{
  def resultName: ResultNameSQLSyntaxProvider[S, A] = ???
}

def include(syntaxProviders: QuerySQLSyntaxProvider[_, _]*) = {
  syntax(syntaxProviders.map(_.resultName)*)
}

def syntax(resultNames: ResultNameSQLSyntaxProvider[_, _]*) = ???