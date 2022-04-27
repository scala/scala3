package example

type Elem/*<-example::MatchType$package.Elem#*/[X/*<-example::MatchType$package.Elem#[X]*/] = X/*->example::MatchType$package.Elem#[X]*/ match
  case String/*->scala::Predef.String#*/ => Char/*->scala::Char#*/
  case Array/*->scala::Array#*/[t/*<-local0*/] => t/*->local0*/
  case Iterable/*->scala::package.Iterable#*/[t/*<-local1*/] => t/*->local1*/

type Concat/*<-example::MatchType$package.Concat#*/[Xs/*<-example::MatchType$package.Concat#[Xs]*/ <: Tuple/*->scala::Tuple#*/, +Ys/*<-example::MatchType$package.Concat#[Ys]*/ <: Tuple/*->scala::Tuple#*/] <: Tuple/*->scala::Tuple#*/ = Xs/*->example::MatchType$package.Concat#[Xs]*/ match
  case EmptyTuple/*->scala::Tuple$package.EmptyTuple#*/ => Ys/*->example::MatchType$package.Concat#[Ys]*/
  case x/*<-local2*/ *:/*->scala::`*:`#*/ xs/*<-local3*/ => x/*->local2*/ *:/*->scala::`*:`#*/ Concat/*->example::MatchType$package.Concat#*/[xs/*->local3*/, Ys/*->example::MatchType$package.Concat#[Ys]*/]
