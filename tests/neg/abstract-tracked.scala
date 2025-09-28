import scala.language.experimental.modularity
import scala.language.future

tracked trait F

trait G:
  tracked def f: F // error

tracked object O // error

tracked class C

def f =
  tracked val x = 1 // error
