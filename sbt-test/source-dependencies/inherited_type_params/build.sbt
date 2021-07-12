name := "test"

TaskKey[Unit]("check-same") := {
  val analysis = (compile in Compile).value.asInstanceOf[sbt.internal.inc.Analysis]
  analysis.apis.internal.foreach { case (_, api) =>
    assert(xsbt.api.SameAPI(api.api, api.api))
  }
}
