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
 *  @myMain def sum(first: Int, second: Int = 0, rest: Int*): Int = first + second + rest.sum
 *  ```
 *  generates
 *  ```scala
 *  object foo {
 *    def main(args: Array[String]): Unit = {
 *      val mainAnnot = new myMain()
 *      val info = new Info(
 *        name = "foo.main",
 *        documentation = "Sum all the numbers",
 *        parameters = Seq(
 *          new Parameter("first", "scala.Int", hasDefault=false, isVarargs=false, "Fist number to sum"),
 *          new Parameter("rest", "scala.Int" , hasDefault=false, isVarargs=true, "The rest of the numbers to sum")
 *        )
 *      )
 *      val mainArgsOpt = mainAnnot.command(info, args)
 *      if mainArgsOpt.isDefined then
 *        val mainArgs = mainArgsOpt.get
 *        val args0 = mainAnnot.argGetter[Int](info.parameters(0), mainArgs(0), None) // using parser Int
 *        val args1 = mainAnnot.argGetter[Int](info.parameters(1), mainArgs(1), Some(() => sum$default$1())) // using parser Int
 *        val args2 = mainAnnot.varargGetter[Int](info.parameters(2), mainArgs.drop(2)) // using parser Int
 *        mainAnnot.run(() => sum(args0(), args1(), args2()*))
 *    }
 *  }
 *  ```
 *
 *  @param Parser The class used for argument string parsing and arguments into a `T`
 *  @param Result The required result type of the main method.
 *                If this type is Any or Unit, any type will be accepted.
 */
@experimental
trait MainAnnotation[Parser[_], Result] extends StaticAnnotation:
  import MainAnnotation.{Info, Parameter}

  /** Process the command arguments before parsing them.
   *
   *  Return `Some` of the sequence of arguments that will be parsed to be passed to the main method.
   *  This sequence needs to have the same length as the number of parameters of the main method (i.e. `info.parameters.size`).
   *  If there is a varags parameter, then the sequence must be at least of length `info.parameters.size - 1`.
   *
   *  Returns `None` if the arguments are invalid and parsing and run should be stopped.
   *
   *  @param info The information about the command (name, documentation and info about parameters)
   *  @param args The command line arguments
   */
  def command(info: Info, args: Seq[String]): Option[Seq[String]]

  /** The getter for the `idx`th argument of type `T`
   *
   *   @param idx The index of the argument
   *   @param defaultArgument Optional lambda to instantiate the default argument
   */
  def argGetter[T](param: Parameter, arg: String, defaultArgument: Option[() => T])(using Parser[T]): () => T

  /** The getter for a final varargs argument of type `T*` */
  def varargGetter[T](param: Parameter, args: Seq[String])(using Parser[T]): () => Seq[T]

  /** Run `program` if all arguments are valid if all arguments are valid
   *
   *  @param program A function containing the call to the main method and instantiation of its arguments
   */
  def run(program: () => Result): Unit

end MainAnnotation

@experimental
object MainAnnotation:

  /** Information about the main method
   *
   *  @param name The name of the main method
   *  @param documentation The documentation of the main method without the `@param` documentation (see Parameter.documentaion)
   *  @param parameters Information about the parameters of the main method
   */
  final class Info(
    val name: String,
    val documentation: String,
    val parameters: Seq[Parameter],
  ):

    /** If the method ends with a varargs parameter */
    def hasVarargs: Boolean = parameters.nonEmpty && parameters.last.isVarargs

  end Info

  /** Information about a parameter of a main method
   *
   *  @param name The name of the parameter
   *  @param typeName The name of the parameter's type
   *  @param hasDefault If the parameter has a default argument
   *  @param isVarargs If the parameter is a varargs parameter (can only be true for the last parameter)
   *  @param documentation The documentation of the parameter (from `@param` documentation in the main method)
   *  @param annotations The annotations of the parameter that extend `ParameterAnnotation`
   */
  final class Parameter(
    val name: String,
    val typeName: String,
    val hasDefault: Boolean,
    val isVarargs: Boolean,
    val documentation: String,
    val annotations: Seq[ParameterAnnotation],
  )

  /** Marker trait for annotations that will be included in the Parameter annotations. */
  trait ParameterAnnotation extends StaticAnnotation

end MainAnnotation
