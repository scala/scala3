/**
 * JAR used for testing repl :jar
 * Generated using: mkdir out; scalac -d out MyLibrary.scala; jar cf mylibrary.jar -C out .
 */
package mylibrary

object Utils:
  def greet(name: String): String = s"Hello, $name!"
