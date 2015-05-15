class a { type X = Int }

object Test extends dotty.runtime.LegacyApp {
  Array(1) match { case _: Array[a#X] => }
}
