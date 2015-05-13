object Test extends dotty.runtime.LegacyApp{
  val q : Queryable[Any] = new Queryable[Any]
  q.map(e1 => q.map(e2=>e1))

  locally {
    val q : Queryable[Any] = new Queryable[Any]
    q.map(e1 => q.map(e2=>e1))
  }
}