object Usage:
  case class Bar(x: Int, y: String, z: (Double, Double))
  MacroUtils.extractNameFromSelector[Bar, String](_.y + "abc")
end Usage
