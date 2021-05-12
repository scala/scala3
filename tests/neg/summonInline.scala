import scala.compiletime.summonInline
import scala.annotation.implicitNotFound

@implicitNotFound("Missing One")
trait Missing1

@implicitNotFound("Missing Two")
trait Missing2

trait NotMissing
given NotMissing = ???

transparent inline def summonInlineCheck[T <: Int](inline t : T) : Any = 
  inline t match 
    case 1 => summonInline[Missing1]
    case 2 => summonInline[Missing2]
    case _ => summonInline[NotMissing]
    
val missing1 = summonInlineCheck(1) // error
val missing2 = summonInlineCheck(2) // error
val notMissing : NotMissing = summonInlineCheck(3)