name := "test"

TaskKey[Unit]("check-same") := Def.uncached {
  val analysis = (Compile / compile).value.asInstanceOf[sbt.internal.inc.Analysis]
  analysis.apis.internal.foreach { case (_, api) =>
    assert(xsbt.api.SameAPI(api.api, api.api))
  }
}
