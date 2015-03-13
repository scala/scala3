object test {
  class Foo[T](val arg : T)

  object Foo  {
    def unapply [a](m : Foo[a]) = Some (m.arg)
  }
  def matchAndGetArgFromFoo[b]( e:Foo[b]):b = {e match { case Foo(x) => x }}
// Unapply node here will have type argument [a] instantiated to scala.Nothing:
// UnApply(TypeApply(Select(Ident(Foo),unapply),List(TypeTree[TypeVar(PolyParam(a) -> TypeRef(ThisType(TypeRef(NoPrefix,scala)),Nothing))])),List(),List(Bind(x,Ident(_))))
// but the type of the UnApply node itself is correct: RefinedType(TypeRef(ThisType(TypeRef(ThisType(TypeRef(NoPrefix,<empty>)),test$)),Foo), test$$Foo$$a, TypeAlias(TypeRef(NoPrefix,a)))
}
