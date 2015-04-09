object test {

        class Foo[a](val arg : a)

        object Foo  {
        def apply [a](arg : a, right :a) = new Foo[a](arg)
        def unapply [a](m : Foo[a]) = Some (m.arg)
        }

    def matchAndGetArgFromFoo[a]( e:Foo[a]):a = {e match { case Foo(x) => x }}
  // Unapply node here will have type argument [a] instantiated to scala.Nothing:
  // UnApply(TypeApply(Select(Ident(Foo),unapply),List(TypeTree[TypeVar(PolyParam(a) -> TypeRef(ThisType(TypeRef(NoPrefix,scala)),Nothing))])),List(),List(Bind(x,Ident(_))))
  // but the type of the UnApply node itself is correct: RefinedType(TypeRef(ThisType(TypeRef(ThisType(TypeRef(NoPrefix,<empty>)),test$)),Foo), test$$Foo$$a, TypeAlias(TypeRef(NoPrefix,a)))
  //


    //  Try the same thing as above but use function as argument to Bar
    // constructor

    type FunIntToA [a] = (Int) => a
        class Bar[a] (var f: FunIntToA[a])

    object Bar {
        def apply[a](f: FunIntToA[a]) = new Bar[a](f)
        def unapply[a](m: Bar[a]) = Some (m.f)
    }

    def matchAndGetFunFromBar[a](b:Bar[a]) : FunIntToA[a] = { b match { case Bar(x) => x}}
}
