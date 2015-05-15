object Test extends dotty.runtime.LegacyApp {
  val result = "börk börk" flatMap (ch ⇒ if (ch > 127) f"&#x${ch}%04x;" else "" + ch)
  println(result)
}
