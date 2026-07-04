//> using options -Yno-flexible-types

// Test that null marked scopes are working
import test.*

class S {
  def kk: String = J.k // ok: in null marked scope

  def ll: String = J.l // ok: in null marked scope

  def kk2: String = J.J2.k2 // error: in unmarked scope

  def ll2: String = J.J2.l2 // error: in unmarked scope
}