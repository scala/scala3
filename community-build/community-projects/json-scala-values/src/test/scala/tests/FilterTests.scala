package tests

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import json.value.*
import json.value.Conversions.given
import scala.language.implicitConversions
class FilterTests extends AnyFlatSpec with should.Matchers{


  "filterKeys" should "remove remove selected keys" in {

    val xs = JsObj("a" -> 1, "b" -> 2, "C" -> "3", "d" -> JsArray(JsObj("E"-> 1, "f"-> 2),1), "G" -> 3)

    val expected = JsObj("a" -> 1, "b" -> 2, "d" -> JsArray(JsObj("f"-> JsInt(2)),1))

    xs.filterKeys(it=>it.forall(_.isLower)) should be(expected)
  }

  "filterKeys by path" should "remove remove selected paths" in {
    val xs = JsObj("a" -> 1, "b" -> 2, "C" -> "3", "d" -> JsArray(JsObj("E" -> 1, "f" -> 2), 1), "G" -> 3)

    val expected = JsObj("a" -> 1, "b" -> 2, "d" -> JsArray(JsObj("f" -> JsInt(2)), 1))

    xs.filterKeys((p,v) => {
      if xs(p) != v then throw RuntimeException()
      p.lastKey.forall(_.isLower)
    }) should be(expected)
  }


  "filterValues" should "remove selected values" in {
    val xs = JsObj("a" -> 1, "b" -> 2, "C" -> "3", "d" -> JsArray(JsObj("E" -> 1, "f" -> 2), 1), "G" -> 3, "H" -> 1)

    val expected = JsObj("b" -> 2,  "d" -> JsArray(JsObj("f" -> JsInt(2))), "G" -> 3)

    xs.filter(JsInt.prism.exist(_!=1)) should be(expected)
  }

  "filterValues by path" should "remove selected values" in {
    val xs = JsObj("a" -> 1, "b" -> 2, "C" -> "3", "d" -> JsArray(JsObj("E" -> 2, "f" -> 2), 1), "G" -> 3, "H" -> 1)

    val expected = JsObj("d" -> JsArray(JsObj("E" -> JsInt(2))),  "G" -> 3)

    val ys = xs.filter((p,v) => {
      if xs(p) != v then throw RuntimeException()
      JsInt.prism.exist(_ != 1)(v) && Key.prism.exist(name =>name.forall(_.isUpper))(p.last)
    }
    )

    ys should be(expected)
  }

}
