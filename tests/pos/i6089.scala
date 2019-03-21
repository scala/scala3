trait X { override def toString = super.toString + " X" }
trait Y { override def toString = super.toString + " Y" }
class Z extends X with Y
