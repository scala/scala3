//> using options -experimental -language:experimental.erasedDefinitions

class IsOn[T]
type On
object IsOn {
  erased given IsOn[On] = new IsOn[On]
}
