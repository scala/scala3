class Repo[T]:
  val ctx = new Context
  inline def summonEncoder = { import ctx.*
    summonMyEncoder[T]
  }

object Use:
  val repo = new Repo[String]
  val v = repo.summonEncoder // used to crash the compiler: exception while typing Repo.this
