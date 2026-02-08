//> using options -deprecation
package p:
  trait Qlike:
    type Y
    type Z

package p.q:
  object `package` extends p.Qlike:
    override trait Y // warn
    override class Z // warn
