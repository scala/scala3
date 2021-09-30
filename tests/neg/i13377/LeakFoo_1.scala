import scala.util.NotGiven
type LeakFoo[T] = core.LeakingFoo[T]
val ok = summon[NotGiven[LeakFoo[1] =:= LeakFoo[2]]]
