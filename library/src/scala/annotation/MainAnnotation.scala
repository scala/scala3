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
  def command(args: Array[String], commandName: String, documentation: String): MainAnnotation.Command[ArgumentParser, MainResultType]
end MainAnnotation

object MainAnnotation:
  // Inspired by https://github.com/scala-js/scala-js/blob/0708917912938714d52be1426364f78a3d1fd269/linker-interface/shared/src/main/scala/org/scalajs/linker/interface/StandardConfig.scala#L23-L218
  final class ParameterInfos[T] private (
    /** The name of the parameter */
    val name: String,
    /** The name of the parameter's type */
    val typeName: String,
    /** The docstring of the parameter. Defaults to None. */
    val documentation: Option[String],
    /** The default value that the parameter has. Defaults to None. */
    val defaultValueGetterOpt: Option[() => T],
    /** The ParameterAnnotations associated with the parameter. Defaults to Seq.empty. */
    val annotations: Seq[ParameterAnnotation],
  ) {
    // Main public constructor
    def this(name: String, typeName: String) =
      this(name, typeName, None, None, Seq.empty)

    def withDefaultValue(defaultValueGetter: () => T): ParameterInfos[T] =
      new ParameterInfos(name, typeName, documentation, Some(defaultValueGetter), annotations)

    def withDocumentation(doc: String): ParameterInfos[T] =
      new ParameterInfos(name, typeName, Some(doc), defaultValueGetterOpt, annotations)

    def withAnnotations(annots: ParameterAnnotation*): ParameterInfos[T] =
      new ParameterInfos(name, typeName, documentation, defaultValueGetterOpt, annots)

    override def toString: String = s"$name: $typeName"
  }

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
