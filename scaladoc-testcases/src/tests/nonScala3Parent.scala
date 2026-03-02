package tests
package nonScala3Parent

import javax.swing.JPanel
import javax.swing.JFrame

// https://github.com/scala/scala3/issues/15927

trait Foo1 extends Numeric[Any]
trait Foo2 extends JPanel
trait Foo3 extends JFrame
trait Foo4 extends Ordering[Any]
trait Foo5 extends Enumeration
