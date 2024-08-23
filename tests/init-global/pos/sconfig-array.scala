import java.{lang => jl}

final class ConfigValueType private (val name: String, val ordinal: Int)

object ConfigValueType:
  final val OBJECT = new ConfigValueType("OBJECT", 0)
  final val LIST = new ConfigValueType("LIST", 1)
  final val NUMBER = new ConfigValueType("NUMBER", 2)
  final val BOOLEAN = new ConfigValueType("BOOLEAN", 3)
  final val NULL = new ConfigValueType("NULL", 4)
  final val STRING = new ConfigValueType("STRING", 5)

  final val _values: Array[ConfigValueType] =
    Array(OBJECT, LIST, NUMBER, BOOLEAN, NULL, STRING)

  def values: Array[ConfigValueType] = _values.clone()

  def valueOf(name: String): ConfigValueType =
    _values.find(_.name == name).getOrElse {
      throw new IllegalArgumentException(
        "No enum const ConfigValueType." + name
      )
    }

object Usage:
  val c = ConfigValueType.valueOf("LIST")
