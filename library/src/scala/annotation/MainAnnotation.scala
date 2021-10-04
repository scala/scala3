package scala.annotation

/** MainAnnotation provides the functionality for a compiler-generated main class.
 *  It links a compiler-generated main method (call it compiler-main) to a user
 *  written main method (user-main).
 *  The protocol of calls from compiler-main is as follows:
 *
 *    - create a `command` with the command line arguments,
 *    - for each parameter of user-main, a call to `command.argGetter`,
 *      or `command.varargGetter` if is a final varargs parameter,
 *    - a call to `command.run` with the closure of user-main applied to all arguments.
 *
 *  Example:
 *  ```scala
 *  /** Sum all the numbers
 *   *
 *   *  @param first Fist number to sum
 *   *  @param rest The rest of the numbers to sum
 *   */
 *  @myMain def sum(first: Int, rest: Int*): Int = first + rest.sum
 *  ```
 *  generates
 *  ```scala
 *  object foo {
 *    def main(args: Array[String]): Unit = {
 *      val cmd = new myMain().command(
 *        info = new CommandInfo(
 *          name = "foo.main",
 *          documentation = "Sum all the numbers",
 *          parameters = Seq(
 *            new ParameterInfo("first", "scala.Int", hasDefault=false, isVarargs=false, "Fist number to sum"),
 *            new ParameterInfo("rest", "scala.Int" , hasDefault=false, isVarargs=true, "The rest of the numbers to sum")
 *          )
 *        )
 *        args = args
 *      )
 *      val args0 = cmd.argGetter[Int](0, None) // using cmd.Parser[Int]
 *      val args1 = cmd.varargGetter[Int] // using cmd.Parser[Int]
 *      cmd.run(() => sum(args0(), args1()*))
 *    }
 *  }
 *  ```
 *
 */
@experimental
trait MainAnnotation extends StaticAnnotation:
  import MainAnnotation.{Command, CommandInfo}

  /** A new command with arguments from `args`
   *
   *  @param info The information about the command (name, documentation and info about parameters)
   *  @param args The command line arguments
   */
  def command(info: CommandInfo, args: Array[String]): Command[?, ?]

end MainAnnotation

@experimental
object MainAnnotation:

  /** A class representing a command to run
   *
   *  @param Parser The class used for argument string parsing and arguments into a `T`
   *  @param Result The required result type of the main method.
   *                If this type is Any or Unit, any type will be accepted.
   */
  trait Command[Parser[_], Result]:

    /** The getter for the `idx`th argument of type `T`
     *
     *   @param idx The index of the argument
     *   @param defaultArgument Optional lambda to instantiate the default argument
     */
    def argGetter[T](idx: Int, defaultArgument: Option[() => T])(using Parser[T]): () => T

    /** The getter for a final varargs argument of type `T*` */
    def varargGetter[T](using Parser[T]): () => Seq[T]

    /** Run `program` if all arguments are valid if all arguments are valid
     *
     *  @param program A function containing the call to the main method and instantiation of its arguments
     */
    def run(program: () => Result): Unit
  end Command

  /** Information about the main method
   *
   *  @param name The name of the main method
   *  @param documentation The documentation of the main method without the `@param` documentation (see ParameterInfo.documentaion)
   *  @param parameters Information about the parameters of the main method
   */
  final class CommandInfo(
    val name: String,
    val documentation: String,
    val parameters: Seq[ParameterInfo],
  )

  /** Information about a parameter of a main method
   *
   *  @param name The name of the parameter
   *  @param typeName The name of the parameter's type
   *  @param hasDefault If the parameter has a default argument
   *  @param isVarargs If the parameter is a varargs parameter (can only be true for the last parameter)
   *  @param documentation The documentation of the parameter (from `@param` documentation in the main method)
   *  @param annotations The annotations of the parameter that extend `ParameterAnnotation`
   */
  final class ParameterInfo (
    val name: String,
    val typeName: String,
    val hasDefault: Boolean,
    val isVarargs: Boolean,
    val documentation: String,
    val annotations: Seq[ParameterAnnotation],
  )

  /** Marker trait for annotations that will be included in the ParameterInfo annotations. */
  trait ParameterAnnotation extends StaticAnnotation

end MainAnnotation
