//> using options -deprecation -Wunused:nowarn

import scala.annotation.nowarn

@deprecated
class A

@deprecated
class B

@nowarn("msg=class A is deprecated")
@nowarn("cat=deprecation&msg=class A is deprecated") // warn @nowarn annotation does not suppress any warnings
@nowarn("cat=deprecation&msg=class B is deprecated")
trait C1 {
  def a: A
  def b: B
}

@nowarn("cat=deprecation&msg=class B is deprecated")
@nowarn("cat=deprecation&msg=class B is deprecated") // warn @nowarn annotation does not suppress any warnings
@nowarn("cat=deprecation&msg=class A is deprecated")
trait C2 {
  def a: A
  def b: B
}
