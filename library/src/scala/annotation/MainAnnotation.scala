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
  def command(args: Array[String], commandName: String, docComment: String): Command

  /** A class representing a command to run */
  abstract class Command(val commandName: String, val docComment: String):

    /** The getter for the next argument of type `T` */
    def argGetter[T](argName: String, argType: String, argDoc: String)(using fromString: ArgumentParser[T]): () => T

    /** The getter for the next argument of type `T` with a default value */
    def argGetterDefault[T](argName: String, argType: String, argDoc: String, defaultValue: T)(using fromString: ArgumentParser[T]): () => T

    /** The getter for a final varargs argument of type `T*` */
    def argsGetter[T](argName: String, argType: String, argDoc: String)(using fromString: ArgumentParser[T]): () => Seq[T]

    /** Run `program` if all arguments are valid,
     *  or print usage information and/or error messages.
     */
    def run(program: => MainResultType): Unit
  end Command
end MainAnnotation
