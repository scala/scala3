//> using options -deprecation -Wunused:nowarn

import scala.annotation.{nowarn, Annotation}

def t1a = try 1 // warn (parser)
@nowarn("msg=try without catch") def t1b = try 1

@nowarn("wat?") // warn (typer, invalid filter)
def t2 = { 1; 2 } // warn (the invalid nowarn doesn't silence anything)

@nowarn(t1a.toString) // warn (typer, argument not a compile-time constant)
def t2a = { 1; 2 } // warn (invalid nowarn doesn't silence)

object o:
  final val const = "msg=try"
  inline def inl = "msg=try"

@nowarn(o.const) def t2c = try 1 // no warn
@nowarn(o.inl) def t2d = try 1   // warn // warn (`inl` is not a compile-time constant)

@nowarn("id=E129") def t3a = { 1; 2 }
@nowarn("name=PureExpressionInStatementPosition") def t3b = { 1; 2 }

@nowarn("id=E002") def t4a = try 1
@nowarn("id=E2") def t4b = try 1
@nowarn("id=2") def t4c = try 1
@nowarn("id=1") def t4d = try 1 // warn // warn (unused nowarn, wrong id)

@nowarn("verbose") def t5 = try 1 // warn with details

@deprecated def f = 0

def t6a = f // warn (refchecks, deprecation)
@nowarn("cat=deprecation") def t6b = f
@nowarn("msg=deprecated") def t6c = f
@nowarn("msg=fish") def t6d = f // warn (unused nowarn) // warn (deprecation)
@nowarn("") def t6e = f
@nowarn def t6f = f

def t7a = f: @nowarn("cat=deprecation")
def t7b = f
  : @nowarn("msg=deprecated")
def t7c = f             // warn (deprecation)
  : @nowarn("msg=fish") // warn (unused nowarn)
def t7d = f: @nowarn("")
def t7e = f: @nowarn

def t8a(x: Any) = x match
  case _: List[Int] => 0 // warn (patmat, unchecked)
  case _ => 1

@nowarn("cat=unchecked") def t8(x: Any) = x match
  case _: List[Int] => 0
  case _ => 1

@nowarn def t9a = { 1: @nowarn; 2 } // warn (outer @nowarn is unused)
@nowarn def t9b = { 1: Int @nowarn; 2 } // warn (inner @nowarn is unused, it covers the type, not the expression)

class ann(a: Any) extends Annotation

@ann(f) def t10a = 0          // should be a deprecation warning, but currently isn't
@nowarn @ann(f) def t10b = 0  // warn (unused nowarn)
@ann(f: @nowarn) def t10c = 0 // warn (unused nowarn), should be silent

def forceCompletionOfI1a = (new I1a).m
@nowarn class I1a { // warn (unused nowarn)
  @nowarn def m = { 1; 2 }
}

// completion during type checking
@nowarn class I1b { // warn (unused nowarn)
  @nowarn def m = { 1; 2 }
}

@nowarn class I1c {
  def m = { 1; 2 }
}

trait T {
  @nowarn val t1 = { 0; 1 }
}

class K extends T
