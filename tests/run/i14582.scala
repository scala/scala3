// https://github.com/scala/scala3/issues/14582
@main def Test() =
  val map = Map(
    "a" -> 1,
    "b" -> 2
  )

  val mapView = map.view

  val optionMapView = Some(mapView)

  val listOfTuples: List[(String, String)] = List(("c", "d"), ("e", "f"))

  val mapViewWithDefault = optionMapView.getOrElse(Map())

  val result = mapViewWithDefault ++ listOfTuples

  result.toSeq
