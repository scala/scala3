// scalac -Wunsued:all
//Avoid warning on setter in trait Regression test : issue10154 scala

trait T {
  private var x: String = _

  def y: String = {
    if (x eq null) x = "hello, world"
    x
  }
}

/*
➜  skalac -version
Scala compiler version 2.13.10-20220920-001308-98972e5 -- Copyright 2002-2022, LAMP/EPFL and Lightbend, Inc.

➜  skalac -d /tmp -Wunused -Vprint:typer t12646.scala
t12646.scala:3: warning: parameter value x_= in variable x is never used
  private var x: String = _
              ^
[[syntax trees at end of                     typer]] // t12646.scala
package <empty> {
  abstract trait T extends scala.AnyRef {
    def /*T*/$init$(): Unit = {
      ()
    };
    <accessor> private val x: String = _;
    <accessor> private def x_=(x$1: String): Unit;
    def y: String = {
      if (T.this.x.eq(null))
        T.this.x_=("hello, world")
      else
        ();
      T.this.x
    }
  }
}

1 warning
*/
