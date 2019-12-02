object foo {
  def main(args: Array[String]): Unit = println("Dotty")

  import java.net.URL
  import java.util.List

  trait ConfigOrigin {
    def description: String
    def filename: String
    def url: URL
    def resource: String
    def lineNumber: Int
    def comments: List[String]
    def withComments(comments: List[String]): ConfigOrigin
    def withLineNumber(lineNumber: Int): ConfigOrigin
  }


  trait ConfigValue extends ConfigMergeable {
    def origin: ConfigOrigin
    //def valueType: ConfigValueType
    def unwrapped: AnyRef
    def render: String
    //def render(options: ConfigRenderOptions): String
    override def withFallback(other: ConfigMergeable): ConfigValue
    //def atPath(path: String): Config
    //def atKey(key: String): Config
    def withOrigin(origin: ConfigOrigin): ConfigValue
  }


  trait ConfigMergeable {
    def withFallback(other: ConfigMergeable): ConfigMergeable
  }

  trait MergeableValue extends ConfigMergeable {
    def toFallbackValue(): ConfigValue
  }

  // no impl
  final class SimpleConfigOrigin extends ConfigOrigin {
    def comments: java.util.List[String] = ???
    def description: String = ???
    def filename: String = ???
    def lineNumber: Int = ???
    def resource: String = ???
    def url: java.net.URL = ???
    def withComments(comments: java.util.List[String]): foo.ConfigOrigin = ???
    def withLineNumber(lineNumber: Int): foo.ConfigOrigin = ???
  }

  abstract class AbstractConfigValue (val _origin: ConfigOrigin)
    extends ConfigValue
    with MergeableValue {

  override def origin: SimpleConfigOrigin =
    this._origin.asInstanceOf[SimpleConfigOrigin]
  }
}