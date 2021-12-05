package scala.annotation

/** MainAnnotation provides the functionality for a compiler-generated main class.
 *  It links a compiler-generated main method (call it compiler-main) to a user
 *  written main method (user-main).
 *  The protocol of calls from compiler-main is as follows:
 *
 *    - create a `command` with the command line arguments,
 *    - for each parameter of user-main, a call to `command.argGetter`,
 *      or `command.argsGetter` if is a final varargs parameter,
 *    - a call to `command.run` with the closure of user-main applied to all arguments.
 */
trait MainAnnotation extends StaticAnnotation:

  /** The class used for argument string parsing. E.g. `scala.util.CommandLineParser.FromString`,
   *  but could be something else
   */
  type ArgumentParser[T]

  /** The required result type of the main function */
  type MainResultType

  /** A new command with arguments from `args` */
  def command(args: Array[String], commandName: String, docComment: String): MainAnnotation.Command[ArgumentParser, MainResultType]
end MainAnnotation

object MainAnnotation:
  /**
    * The information related to one of the parameters of the annotated method.
    * @param name the name of the parameter
    * @param typeName the name of the parameter's type
    * @tparam T the type of the parameter
    */
  class ParameterInfos[T](var name: String, var typeName: String):
    /** The docstring of the parameter. Defaults to None. */
    var documentation: Option[String] = None
    /** The default value that the parameter has. Defaults to None. */
    var defaultValue: Option[T] = None
    /** If there is one, the ParameterAnnotation associated with the parameter. Defaults to None. */
    var annotation: Option[ParameterAnnotation] = None

  /** A class representing a command to run */
  trait Command[ArgumentParser[_], MainResultType]:

    /** The getter for the next argument of type `T` */
    def argGetter[T](paramInfos: ParameterInfos[T])(using fromString: ArgumentParser[T]): () => T

    /** The getter for a final varargs argument of type `T*` */
    def varargGetter[T](paramInfos: ParameterInfos[T])(using fromString: ArgumentParser[T]): () => Seq[T]

    /** Run `program` if all arguments are valid,
     *  or print usage information and/or error messages.
     */
    def run(program: => MainResultType): Unit
  end Command

  /** An annotation for the parameters of a MainAnnotated method. */
  trait ParameterAnnotation extends StaticAnnotation
end MainAnnotation
