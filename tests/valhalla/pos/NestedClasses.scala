import scala.annotation.valhalla

@valhalla
class NestedClasses extends AnyVal{
  class inner

  @valhalla
  class innerValhalla extends AnyVal
}

class NestedIdentityClasses {
  class inner

  @valhalla
  class innerValhalla extends AnyVal
}