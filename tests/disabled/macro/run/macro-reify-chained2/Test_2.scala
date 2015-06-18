object Test extends dotty.runtime.LegacyApp{
  val q : Queryable[Any] = new Queryable[Any]
  q.map(x => x).map(x => x)

  locally {
    val q : Queryable[Any] = new Queryable[Any]
    q.map(x => x).map(x => x)
  }
}