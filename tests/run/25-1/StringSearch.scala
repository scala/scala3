package p
import scala.language.unsafeNulls
trait C[T]
trait Search[M] { def search(input: M): C[Int] = null }
object StringSearch extends Search[String] { }
