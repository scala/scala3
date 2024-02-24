//> using options -source future -feature -Xfatal-warnings

package adhoc
class B extends A  // warn: adhoc-extension (under -strict -feature -Xfatal-warnings)
class C extends A  // warn

object O {
  val a = new A {}  // warn
  object E extends A  // warn
}
// nopos-error: No warnings can be incurred under -Werror (or -Xfatal-warnings)