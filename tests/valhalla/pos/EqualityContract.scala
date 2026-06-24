import scala.annotation.valhalla

@valhalla
case class Pig(val id: Int, val luckyNumber: Int) extends AnyVal with DeepValhalla

class Pig1(val id: Int, val luckyNumber: Int):
    override def equals(that: Any): Boolean = {
        that match {
            case Pig(id1, luckyNumber1) => id == id1 && luckyNumber == luckyNumber1
            case _ => false
        }
    }

class Main:
    def main(args: Array[String]): Unit = {
        val pig1 = new Pig(1, 10)
        val refPig = new Pig1(1, 10)
        if(pig1 != refPig && refPig != pig1) then println("Symmetric!") else println("badbad!")
    }