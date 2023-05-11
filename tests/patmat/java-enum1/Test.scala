class Test:
  private def showParameterModifier(base: String, pm: ParameterModifier): String = pm match {
    case ParameterModifier.Plain    => base
    case ParameterModifier.Repeated => base + "*"
    case ParameterModifier.ByName   => "=> " + base
  }
