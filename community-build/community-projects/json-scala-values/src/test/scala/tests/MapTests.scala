package tests

import json.value.*
import json.value.Conversions.given
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.time.Instant
import scala.language.implicitConversions
import scala.util.{Failure, Try}

class MapTests extends AnyFlatSpec with should.Matchers {
  

  "mapKeys" should "transform all the json keys" in {

    val xs = JsObj("A" -> 1,"B" ->JsArray(JsObj("C" -> 2,"D" -> 3, "E" -> JsObj("F" -> JsNull))))

    val expected = JsObj("a" -> 1, "b" -> JsArray(JsObj("c" -> 2,"d" -> 3, "e" -> JsObj("f" -> JsNull))))

    xs.mapKeys(_.toLowerCase) should be(expected)

    xs.mapKeys((p,v) => {
      if xs(p) != v then throw RuntimeException()
      p.lastKey.nn.toLowerCase
    }, (_,_) => true) should be(expected)



    xs.mapKeys((p, v) => {
      if xs(p) != v then throw RuntimeException()
      p.lastKey.nn.toLowerCase
    }, (p, v) => {
      if xs(p) != v then throw RuntimeException()
      Key.prism.exist(_ == "F")(p.last)
    }) should be(JsObj("A" -> 1,"B" ->JsArray(JsObj("C" -> 2,"D" -> 3, "E" -> JsObj("f" -> JsNull)))))

  }


  "map" should "transform all the json values" in {

    val xs = JsObj("a" -> 1, "b" -> 1,"c"-> JsArray(
        JsObj("d" -> 1,"e"  -> 1),
        JsObj("f" -> 1,"g" -> 1, "h" -> JsArray(JsObj("i" -> 1, "j" -> 1,"k"->"hi"))))
    )

    val expected = JsObj("a" -> 2, "b" -> 2, "c" -> JsArray(
      JsObj("d" -> 2, "e" -> 2),
      JsObj("f" -> 2, "g" -> 2, "h" -> JsArray(JsObj("i" -> 2, "j" -> 2,"k"->"hi")))))

    xs.map(JsInt.prism.modify(_+ 1)) should be(expected)

    xs.map((p,v) => {
      if xs(p) != v then throw RuntimeException()
      JsInt.prism.modify(_+ 1)(v)
    },(p,v) => {
      if xs(p) != v then throw RuntimeException()
      true
    })  should be(expected)

    xs.map((p, v) => {
      if xs(p) != v then throw RuntimeException()
      JsInt.prism.modify(_ + 1)(v)
    }, (p, v) => {
      if xs(p) != v then throw RuntimeException()
      p.lastKey=="j"
    }) should be(JsObj("a" -> 1, "b" -> 1, "c" -> JsArray(JsObj("d" -> 1, "e" -> 1), JsObj("f" -> 1, "g" -> 1, "h" -> JsArray(JsObj("i" -> 1, "j" -> 2, "k" -> "hi"))))))

    xs.map((p, v) => {
      if xs(p) != v then throw RuntimeException()
      p.toString
    }, (p, v) => {
      if xs(p) != v then throw RuntimeException()
      true
    }) should be(
      JsObj("a" -> "a", "b" -> "b", "c" ->
        JsArray(JsObj("d" -> "c / 0 / d", "e" -> "c / 0 / e"),
                JsObj("f" -> "c / 1 / f", "g" -> "c / 1 / g", "h" -> JsArray(JsObj("i" -> "c / 1 / h / 0 / i", "j" -> "c / 1 / h / 0 / j", "k" -> "c / 1 / h / 0 / k"))))))

  }


}