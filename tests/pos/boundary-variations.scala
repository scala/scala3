import util.boundary
import boundary.{break, Label}

def run(f: Int => String, x: Int): String =
  boundary:
    def recur(x: Int): String =
      if x < 0 then break("negative")
      else if x == 0 then ""
      else f(x) ++ " " ++ recur(x - 1)
    recur(x)

def runDyn(f: Int => Label[String] ?=> String, x: Int): String =
  boundary:
    def recur(x: Int): String =
      if x < 0 then break("negative")
      else if x == 0 then ""
      else f(x) ++ " " ++ recur(x - 1)
    recur(x)

def toStr(x: Int)(using lbl: Label[String]): String = x.toString

def middleDyn(x: Int)(using lbl: Label[String]) =
  if x == 1 then break("middleman attack")(using lbl)
  x.toString

case class Break(s: String) extends Exception

def runExc(f: Int => String, x: Int): String =
  try
    def recur(x: Int): String =
      if x < 0 then throw Break("negative")
      else if x == 0 then ""
      else f(x) ++ " " ++ recur(x - 1)
    recur(x)
  catch case Break(s) => s

def middleExc(x: Int) =
  if x == 1 then throw Break("middleman attack")
  x.toString

@main def Test =
  println(run(_.toString, 2))
  println(run(_.toString, -2))

  println(runDyn(toStr, 2))
  println(runDyn(toStr, -2))
  println(runDyn(middleDyn, 2))

  println(runExc(_.toString, 2))
  println(runExc(_.toString, -2))
  println(runExc(middleExc, 2))

