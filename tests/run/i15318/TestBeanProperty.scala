import scala.annotation.meta.beanGetter
import scala.beans.BeanProperty

type BeanGetterJsonProperty = JsonProperty @beanGetter

class TestBeanProperty {

  @(JsonProperty @beanGetter)(value = "REAL_VALUE")
  @BeanProperty
  var value: String = _

  @BeanGetterJsonProperty(value = "REAL_VALUE2")
  @BeanProperty
  var value2: String = _

}
