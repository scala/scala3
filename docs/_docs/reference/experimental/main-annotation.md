---
layout: doc-page
title: "MainAnnotation"
---

`MainAnnotation` provides a generic way to define main annotations such as `@main`.

When a users annotates a method with an annotation that extends `MainAnnotation` a class with a `main` method will be generated. The main method will contain the code needed to parse the command line arguments and run the application.

```scala
/** Sum all the numbers
 *
 *  @param first Fist number to sum
 *  @param rest The rest of the numbers to sum
 */
@myMain def sum(first: Int, rest: Int*): Int = first + rest.sum
```

```scala
object foo {
  def main(args: Array[String]): Unit = {

    val cmd = new myMain().command(
      info = new CommandInfo(
        name = "sum",
        documentation = "Sum all the numbers",
        parameters = Seq(
          new ParameterInfo("first", "scala.Int", hasDefault=false, isVarargs=false, "Fist number to sum", Seq()),
          new ParameterInfo("rest", "scala.Int" , hasDefault=false, isVarargs=true, "The rest of the numbers to sum", Seq())
        )
      ),
      args = args
    )
    val args0 = cmd.argGetter[Int](0, None) // using a parser of Int
    val args1 = cmd.varargGetter[Int] // using a parser of Int
    cmd.run(() => sum(args0(), args1()*))
  }
}
```

The implementation of the `main` method first instantiates the annotation and then creates a `Command`.
When creating the `Command`, the arguments can be checked and preprocessed.
Then it defines a series of argument getters calling `argGetter` for each parameter and `varargGetter` for the last one if it is a varargs. `argGetter` gets an optional lambda that computes the default argument.
Finally, the `run` method is called to run the application. It receives a by-name argument that contains the call the annotated method with the instantiations arguments (using the lambdas from `argGetter`/`varargGetter`).


Example of implementation of `myMain` that takes all arguments positionally. It used `util.CommandLineParser.FromString` and expects no default arguments. For simplicity, any errors in preprocessing or parsing results in crash.

```scala
// Parser used to parse command line arguments
import scala.util.CommandLineParser.FromString[T]

// Result type of the annotated method is Int
class myMain extends MainAnnotation:
  import MainAnnotation.{ ParameterInfo, Command }

  /** A new command with arguments from `args` */
  def command(info: CommandInfo, args: Array[String]): Command[FromString, Int] =
    if args.contains("--help") then
      println(info.documentation)
      // TODO: Print documentation of the parameters
      System.exit(0)
    assert(info.parameters.forall(!_.hasDefault), "Default arguments are not supported")
    val (plainArgs, varargs) =
      if info.parameters.last.isVarargs then
        val numPlainArgs = info.parameters.length - 1
        assert(numPlainArgs <= args.length, "Not enough arguments")
        (args.take(numPlainArgs), args.drop(numPlainArgs))
      else
        assert(info.parameters.length <= args.length, "Not enough arguments")
        assert(info.parameters.length >= args.length, "Too many arguments")
        (args, Array.empty[String])
    new MyCommand(plainArgs, varargs)

  @experimental
  class MyCommand(plainArgs: Seq[String], varargs: Seq[String]) extends Command[FromString, Int]:

    def argGetter[T](idx: Int, defaultArgument: Option[() => T])(using parser: FromString[T]): () => T =
      () => parser.fromString(plainArgs(idx))

    def varargGetter[T](using parser: FromString[T]): () => Seq[T] =
      () => varargs.map(arg => parser.fromString(arg))

    def run(program: () => Int): Unit =
      println("executing program")
      val result = program()
      println("result: " + result)
      println("executed program")
  end MyCommand
end myMain
```
