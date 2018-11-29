trait Search[M] {
  def search(input: M): C[Int] = {
    println("Search received: " + input)
    new C[Int] {} 
  }
}

class SearchC[M] {
  def searchC(input: M): C[Int] = {
    println("SearchC received: " + input)
    new C[Int] {}
  }
}

object StringSearch extends SearchC[String] with Search[String]

trait C[T]
