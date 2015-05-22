

/** Test that the inliner is not inferring that `xs' is
 *  always Nil, removing the call to isEmpty.
 */
object Test extends dotty.runtime.LegacyApp {

  @annotation.tailrec
  def walk(xs: MyList): Unit = {
    if (xs.isEmpty)
      println("empty")
    else {
      println("non-empty")
      walk(MyNil)
    }
  }

  walk(new MyList)
}

class MyList {
  def isEmpty = false
}

object MyNil extends MyList {
  override def isEmpty = true
}

