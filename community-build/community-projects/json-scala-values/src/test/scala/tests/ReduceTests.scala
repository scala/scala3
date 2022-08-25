package tests
import scala.language.implicitConversions
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import json.value.*
import json.value.Conversions.given
class ReduceTests extends AnyFlatSpec with should.Matchers{

  
  "reduce" should "sum the selected values" in {

    val xs = JsObj("a" -> 1, "c" -> JsArray(1,2,3), "b" -> 2,"e" -> JsObj("f" -> JsArray(1,JsObj("g" -> 1,"h" -> 1))))


    xs.reduce[Int]((p,v) => JsInt.prism.getOption(v).isDefined,
                   (p,v) => v.asInstanceOf[JsInt].value,
                   _+_) should be(Some(12))


    xs.reduce[Int]((p, v) => Key.prism.getOption(p.last).isDefined,
                   (p, v) => JsInt.prism.getOption(v).getOrElse(0),
                    _ + _) should be(Some(5))


  }
  
}
