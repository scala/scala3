def Test = {

  type TupleConfig = (Int, String)

  val tConfig = (1, "string")
  val fails = summon[Lens[TupleConfig, Int]].view(tConfig)
}
