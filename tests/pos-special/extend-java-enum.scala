import java.{lang as jl}

final class ConfigSyntax private (name: String, ordinal: Int)
  extends jl.Enum[ConfigSyntax](name, ordinal)

object ConfigSyntax {

  final val JSON = new ConfigSyntax("JSON", 0)
  final val CONF = new ConfigSyntax("CONF", 1)
  final val PROPERTIES = new ConfigSyntax("PROPERTIES", 2)

  private[this] final val _values: Array[ConfigSyntax] =
    Array(JSON, CONF, PROPERTIES)

  def values: Array[ConfigSyntax] = _values.clone()

  def valueOf(name: String): ConfigSyntax =
    _values.find(_.name == name).getOrElse {
      throw new IllegalArgumentException("No enum const ConfigSyntax." + name)
    }
}
