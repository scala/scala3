package dotty.tools.dotc
package config


class Settings {

  protected def defaultClasspath = sys.env.getOrElse("CLASSPATH", ".")

  protected implicit def mkSetting[T](x: T): Setting[T] = new Setting(x)

  var default: Settings = this

  var classpath: Setting[String] = defaultClasspath
  var debug: Setting[Boolean] = false
  var verbose: Setting[Boolean] = false

  var XmaxClassfileName: Setting[Int] = 255

  var YtermConflict: Setting[String] = "error"

  def processArguments(arguments: List[String], processAll: Boolean): (Boolean, List[String]) = ???


}

case class Setting[T](value: T)