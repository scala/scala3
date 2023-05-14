import scala.language.unsafeNulls

import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.util.Using

/** Automate testing debuggability of generated code using JDB and expect
 *
 *  The debugging information is annotated as comments to the code in brackets:
 *
 *  val x = f(3) // [break] [next: line=5]
 *  val y = 5
 *
 *  1. A jdb command must be wrapped in brackets, like `[step]`. All jdb commands can be used.
 *  2. To check output of jdb for a command, use `[cmd: expect]`.
 *  3. If `expect` is wrapped in double quotes, regex is supported.
 *  4. Break commands are collected and set globally.
 *  5. Other commands will be send to jdb in the order they appear in the source file
 *
 *  Note: jdb uses line number starts from 1
 */

object Gen {
  val MainObject = "Test"
  val CommandWait = 1

  sealed trait Tree

  case class Break(line: Int) extends Tree

  case class Command(val name: String, val expect: Expect = EmptyExpect) extends Tree

  sealed trait Expect

  case object EmptyExpect extends Expect

  case class LitExpect(lit: String) extends Expect

  case class PatExpect(pat: String) extends Expect

  case class Program(breaks: Seq[Break], commands: Seq[Command])

  def error(msg: String): Nothing = {
    throw new Exception(msg)
  }

  def parseCommand(command: String, lineNo: Int): Tree = {
    val index = command.indexOf(':')
    if (index == -1) {
      // simple command
      if (command == "break") Break(lineNo)
      else Command(command)
    } else {
      val Seq(cmd, rhs) = command.split(":", 2).toSeq.map(_.trim)
      if (rhs.startsWith("\"")) {
        // regex match
        val content = "\"(.+)\"".r
        rhs match {
          case content(expect) => Command(cmd, PatExpect(expect))
          case _ => error(s"""incorrect specification: `$rhs` for `$cmd` at line $lineNo. Ending " expected.""")
        }
      } else {
        // literal match
        Command(cmd, LitExpect(rhs))
      }
    }
  }

  def parse(file: String): Program = {
    val lines = Using(Source.fromFile(file))(_.getLines().toBuffer).get

    val breaks = new ListBuffer[Break]()
    val cmds = new ListBuffer[Command]()
    lines.zipWithIndex.map { case (code, line) =>
      val comment = if (code.indexOf("//") != -1) code.split("//").last else ""
      val regex = """(?<=\[).*?(?=\])""".r
      for (p <- regex findAllIn comment) parseCommand(p.trim, line + 1) match { // jdb index from 0
        case b: Break   => breaks += b
        case c: Command => cmds += c
      }
    }

    Program(breaks.toList, cmds.toList)
  }

  def generate(program: Program, source: String = "tests/debug/"): String = {
    val Program(breaks, cmds) = program
    val breakpoints = (breaks.map {
      case Break(point) =>
          s"""|send "stop at $MainObject$$:$point\\r"
              |sleep $CommandWait
              |expect "breakpoint $MainObject$$:$point"
              |expect -re $$
              """.stripMargin
    }).mkString("\n\n")

    val commands = (cmds.map {
    case Command(cmd, EmptyExpect)      =>
        s"""|# send_user "send command `$cmd`\\n"
            |send "$cmd\\r"
            |sleep $CommandWait
            |expect -re $$
            """.stripMargin
    case Command(cmd, LitExpect(lit))   =>
        s"""|# send_user "send command `$cmd`\\n"
            |send "$cmd\\r"
            |sleep $CommandWait
            |expect {
            |   "*$lit*" { send_user "success - $cmd : $lit \\n" }
            |   timeout {
            |       send_user "timeout while waiting for response: $cmd : $lit\\n"
            |       exit 1
            |    }
            |}
            |expect -re $$
            |""".stripMargin
    case Command(cmd, PatExpect(pat))   =>
        s"""|# send_user "send command `$cmd`\\n"
            |send "$cmd\\r"
            |sleep $CommandWait
            |expect {
            |   -re {$pat} { send_user "success - $cmd : $pat \\n" }
            |   timeout {
            |       send_user "timeout while waiting for response: $cmd : $pat\\n"
            |       exit 1
            |    }
            |}
            |expect -re $$
            |""".stripMargin
    }).mkString("\n\n")

    s"""|#!/usr/bin/expect
        |
        |# log_user 1
        |# exp_internal 1
        |# set timeout 5
        |
        |send_user "spawning job...\\n"
        |
        |spawn jdb -attach 5005 -sourcepath $source
        |
        |send_user "interacting...\\n"
        |
        |expect {
        |  "*VM Started*" { send_user "success - connected to server \\n" }
        |  timeout {
        |      send_user "timeout while waiting for: *VM Started*\\n"
        |      exit 1
        |  }
        |}
        |
        |send_user "setting breakpoints...\\n"
        |
        |# breakpoints
        |$breakpoints
        |
        |# run
        |send_user "run program...\\n"
        |send "run\\r"
        |expect "Breakpoint hit"
        |
        |# interactions
        |$commands""".stripMargin
  }

  def main(args: Array[String]): Unit = {
    val prog = Gen.parse(args(0))
    // println("--------------------------------")
    // println("prog:" + prog)
    // println("\n\n\n scrip:")
    // println("--------------------------------")
    println(Gen.generate(prog))
  }
}
