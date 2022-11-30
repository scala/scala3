import scala.annotation.meta.beanGetter
import scala.beans.BeanProperty

class TestBeanProperty {

  @(JsonProperty @beanGetter)(value = "REAL_VALUE")
  @BeanProperty
  var value: String = _

}
