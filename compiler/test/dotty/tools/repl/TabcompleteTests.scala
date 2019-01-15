package dotty.tools.repl

import org.junit.Assert._
import org.junit.Test

/** These tests test input that has proved problematic */
class TabcompleteTests extends ReplTest {

  /** Returns the `(<instance completions>, <companion completions>)`*/
  private[this] def tabComplete(src: String)(implicit state: State): List[String] =
    completions(src.length, src, state).map(_.value)

  @Test def tabCompleteList = fromInitialState { implicit s =>
    val comp = tabComplete("List.r")
    assertEquals(List("range"), comp.distinct)
  }

  @Test def tabCompleteListInstance = fromInitialState { implicit s =>
    val comp = tabComplete("(null: List[Int]).sli")
    assertEquals(List("slice", "sliding"), comp.distinct.sorted)
  }

  @Test def tabCompleteModule = fromInitialState{ implicit s =>
    val comp = tabComplete("scala.Pred")
    assertEquals(List("Predef"), comp)
  }

  @Test def tabCompleteInClass = fromInitialState { implicit s =>
    val comp = tabComplete("class Foo { def bar: List[Int] = List.ap")
    assertEquals(List("apply"), comp)
  }

  @Test def tabCompleteTwiceIn = {
    val src1 = "class Foo { def bar(xs: List[Int]) = xs.map"
    val src2 = "class Foo { def bar(xs: List[Int]) = xs.mapC"

    fromInitialState { implicit state =>
      val comp = tabComplete(src1)
      assertEquals(List("map", "mapConserve"), comp.sorted)
      state
    }
    .andThen { implicit state =>
      val comp = tabComplete(src2)
      assertEquals(List("mapConserve"), comp)
    }
  }

  @Test def i3309 = fromInitialState { implicit s =>
    // We make sure we do not crash
    List("\"", ")", "'", "¨", "£", ":", ",", ";", "@", "}", "[", "]", ".")
      .foreach(tabComplete(_))
  }

  @Test def completeFromPreviousState =
    fromInitialState { implicit state  =>
      val src = "class Foo { def comp3 = 3; def comp1 = 1; def comp2 = 2 }"
      run(src)
    }
    .andThen { implicit state =>
      val expected = List("comp1", "comp2", "comp3")
      assertEquals(expected, tabComplete("(new Foo).comp").sorted)
    }

  @Test def completeFromPreviousState2 =
    fromInitialState { implicit state  =>
      val src = "def hello = 1"
      run(src)
    }
    .andThen { implicit state =>
      val expected = List("hello")
      assertEquals(expected, tabComplete("hel"))
    }

  @Test def tabCompleteFromPreviousImport =
    fromInitialState { implicit state =>
      val src = "import java.io.FileDescriptor"
      run(src)
    }
    .andThen { implicit state =>
      val expected = List("FileDescriptor")
      assertEquals(expected, tabComplete("val foo: FileDesc"))
    }

  @Test def tabCompleteRenamedImport =
    fromInitialState { implicit state =>
      val src = "import java.io.{FileDescriptor => Renamed}"
      run(src)
    }
    .andThen { implicit state =>
      val expected = List("Renamed")
      assertEquals(expected, tabComplete("val foo: Rena"))
    }

  @Test def importScala = fromInitialState { implicit s =>
    val comp = tabComplete("import scala.")
    // check that there are no special symbols leaked: <byname>, <special-ops>, ...
    assertEquals(comp.find(_.startsWith("<")), None)
  }
}
