import scala.annotation.meta.{ field as fld, getter as get, setter as set, * }
import scala.beans.BeanProperty

type BeanGetJsonProperty = JsonProperty @beanGetter
type BeanSetJsonProperty = JsonProperty @beanSetter

class Bean {
  @(JsonProperty     ) val normAaa: String = ""
  @(JsonProperty @fld) val normFld: String = ""
  @(JsonProperty @get) val normGet: String = ""
  @(JsonProperty @set) var normSet: String = ""

  @(JsonProperty            ) @BeanProperty val beanAaa: String = ""
  @(JsonProperty @beanGetter) @BeanProperty val beanGet: String = ""
  @(JsonProperty @beanSetter) @BeanProperty var beanSet: String = ""
}

class Bean2 {
  @BeanGetJsonProperty @BeanProperty val beanGet: String = ""
  @BeanSetJsonProperty @BeanProperty var beanSet: String = ""
}
