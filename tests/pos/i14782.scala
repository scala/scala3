object BugExample {
  val urls: List[String] = List("any_url")
  val closures: List[Unit => Unit] =
    urls.map(url => _ => {
      println("Scraping " + url)
    })
  for (closure <- closures) closure(())
}