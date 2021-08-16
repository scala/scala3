import scala.annotation.{ nowarn, Annotation }

// This test doesn't run with `-Werror`, because once there's an error, later phases are skipped and we would not see
// their warnings.
// Instead, this test runs with `-Wunused:nowarn -Wconf:msg=@nowarn annotation does not suppress any warnings:e`.
// Only "unused nowarn" warnings are reported as errors. Since these warnings are reported at the very end, all other
// phases of the compiler run normally.

def t1a = try 1 // warning (parser)
@nowarn("msg=try without catch") def t1b = try 1

@nowarn("wat?") // warning (typer, invalid filter)
def t2 = { 1; 2 } // warning (the invalid nowarn doesn't silence anything)

@nowarn(t1a.toString) // warning (typer, argument not a compile-time constant)
def t2a = { 1; 2 } // warning (invalid nowarn doesn't silence)

@nowarn("id=E129") def t3a = { 1; 2 }
@nowarn("name=PureExpressionInStatementPosition") def t3b = { 1; 2 }

@nowarn("id=E000") def t4a = try 1
@nowarn("id=E0") def t4b = try 1
@nowarn("id=0") def t4c = try 1
@nowarn("id=1") def t4d = try 1 // error and warning (unused nowarn, wrong id)

@nowarn("verbose") def t5 = try 1 // warning with details

@deprecated def f = 0

def t6a = f // warning (refchecks, deprecation)
@nowarn("cat=deprecation") def t6b = f
@nowarn("msg=deprecated") def t6c = f
@nowarn("msg=fish") def t6d = f // error (unused nowarn), warning (deprecation)
@nowarn("") def t6e = f
@nowarn def t6f = f

def t7a = f: @nowarn("cat=deprecation")
def t7b = f:
  @nowarn("msg=deprecated")
def t7c = f:          // warning (deprecation)
  @nowarn("msg=fish") // error (unused nowarn)
def t7d = f: @nowarn("")
def t7e = f: @nowarn

def t8a(x: Any) = x match
  case _: List[Int] => 0 // warning (patmat, unchecked)
  case _ => 1

@nowarn("cat=unchecked") def t8(x: Any) = x match
  case _: List[Int] => 0
  case _ => 1

@nowarn def t9a = { 1: @nowarn; 2 } // error (outer @nowarn is unused)
@nowarn def t9b = { 1: Int @nowarn; 2 } // error (inner @nowarn is unused, it covers the type, not the expression)

class ann(a: Any) extends Annotation

@ann(f) def t10a = 0          // should be a deprecation warning, but currently isn't
@nowarn @ann(f) def t10b = 0  // error (unused nowarn)
@ann(f: @nowarn) def t10c = 0 // error (unused nowarn), should be silent

def forceCompletionOfI1a = (new I1a).m
@nowarn class I1a { // error (unused nowarn)
  @nowarn def m = { 1; 2 }
}

// completion during type checking
@nowarn class I1b { // error (unused nowarn)
  @nowarn def m = { 1; 2 }
}

@nowarn class I1c {
  def m = { 1; 2 }
}

trait T {
  @nowarn val t1 = { 0; 1 }
}

class K extends T
