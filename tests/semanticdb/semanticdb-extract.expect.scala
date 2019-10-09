 /* => _empty_ */  /* => A. */  /* => A.`<init>`(). */ object  /* => java.lang.Object#`<init>`(). */ A /* <= A. */  {

   /* => scala.package.Serializable# */  /* => scala. */  /* => _root_ */  /* => A. */ def foo /* <= A.foo(+1). */ (x /* <= A.foo(+1).(x) */ : Int /* => scala.Int# */ ) = ()
  def foo /* <= A.foo(+0). */ (): Unit /* => scala.Unit# */  = ()

  foo /* => A.foo(+1). */ (1)
  foo /* => A.foo(+0). */ ()

  "".substring /* => java.lang.String#substring(+1). */ (1)
  "".substring /* => java.lang.String#substring(+0). */ (1, 2)

  List /* => scala.package.List. */  /* => scala.collection.IterableFactory#apply(). */ (1, 2)
  List /* => scala.package.List. */ .apply /* => scala.collection.IterableFactory#apply(). */ ()
  List /* => scala.package.List. */ .`apply` /* => scala.collection.IterableFactory#apply(). */ ()
  println /* => scala.Predef.println(+0). */ (1 + /* => scala.Int#`+`(+3). */  2)

   /* => A.Foo#`<init>`(). */  /* => A.Foo#copy().(x) */  /* => A.Foo.`<init>`(). */  /* => A.Foo.apply().(x) */ case class  /* => java.lang.Object#`<init>`(). */ Foo /* <= A.Foo# */ (x: In /* <= A.Foo#`<init>`(). */  /* <= A.Foo#`<init>`().(x) */  /* <= A.Foo#(x) */  /* <= A.Foo#copy().(x) */  /* <= A.Foo.apply().(x) */ t /* => scala.Int# */  /* => A.Foo#(x) */  /* => scala.package.Serializable# */  /* => scala. */  /* => _root_ */ ) /* => scala.Product# */  /* => scala. */  /* => _root_ */  /* => scala.package.Serializable# */
}