/**
 * JAR used for testing repl :jar
 * Generated using: mkdir out2; scalac -d out MyLibrary2.scala; jar cf mylibrary2.jar -C out2 .
 */
package mylibrary2

object Utils2:
  def greet(name: String): String = s"Greetings, $name!"

