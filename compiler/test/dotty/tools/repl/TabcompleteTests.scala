package dotty.tools.repl

import org.junit.Assert._
import org.junit.Test

/** These tests test input that has proved problematic */
class TabcompleteTests extends ReplTest {

  /** Returns the `(<instance completions>, <companion completions>)`*/
  private def tabComplete(src: String)(implicit state: State): List[String] =
    completions(src.length, src, state).map(_.value).sorted

  @Test def tabCompleteList = fromInitialState { implicit s =>
    val comp = tabComplete("List.r")
    assertEquals(List("range"), comp.distinct)
  }

  @Test def tabCompleteListInstance = fromInitialState { implicit s =>
    val comp = tabComplete("(null: List[Int]).sli")
    assertEquals(List("slice", "sliding"), comp.distinct)
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
      assertEquals(List("map", "mapConserve"), comp)
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
      assertEquals(expected, tabComplete("(new Foo).comp"))
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

  @Test def tabClosureComplete = fromInitialState { implicit s =>
    assertEquals(List("map", "mapConserve"), tabComplete("Nil.map"))
    assertEquals(List("map", "mapConserve"), tabComplete("(x: Int => Int) => Nil.map"))
    assertEquals(List("apply"), tabComplete("(x: Int => Int) => x.ap"))
  }

  @Test def importScala = fromInitialState { implicit s =>
    val comp = tabComplete("import scala.")
    // check that there are no special symbols leaked: <byname>, <special-ops>, ...
    assertEquals(Some("<:<"), comp.find(_.startsWith("<")))
    assert(!comp.contains("package"))
  }

  @Test def `null` = fromInitialState { implicit s =>
    val comp = tabComplete("null.")
    assertEquals(
      List("!=", "##", "==", "asInstanceOf", "eq", "equals", "getClass", "hashCode",
          "isInstanceOf", "ne", "notify", "notifyAll", "synchronized", "toString", "wait"),
      comp.distinct.sorted)
  }

  @Test def anyRef = fromInitialState { implicit s =>
    val comp = tabComplete("(null: AnyRef).")
    assertEquals(
      List("!=", "##", "->", "==", "asInstanceOf", "ensuring", "eq", "equals", "formatted",
          "getClass", "hashCode", "isInstanceOf", "ne", "nn", "notify", "notifyAll", "synchronized", "toString", "wait", "→"),
      comp.distinct.sorted)
  }

  @Test def `???` = fromInitialState { implicit s =>
    val comp = tabComplete("???.")
    assertEquals(Nil, comp)
  }

  @Test def moduleCompletion = fromInitialState { implicit s =>
    assertEquals(List("Predef"), tabComplete("object Foo { type T = Pre"))
  }

  @Test def i6415 = fromInitialState { implicit s =>
    assertEquals(List("Predef"), tabComplete("object Foo { opaque type T = Pre"))
  }

  @Test def i12600 = fromInitialState { implicit s =>
    assertEquals(List("select", "show", "simplified", "substituteTypes"),
      tabComplete("import quoted.* ; def fooImpl(using Quotes): Expr[Int] = { import quotes.reflect.* ; TypeRepr.of[Int].s"))
  }

}
