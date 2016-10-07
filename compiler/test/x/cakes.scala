package test

trait API {

  type Name >: Null <: NameAPI
  type Symbol >: Null <: SymbolAPI
  type ScopeEntry >: Null <: ScopeEntryAPI

  class NameAPI
  class SymbolAPI
  class ScopeEntryAPI

}

trait Names { self: SymTab =>

  class Name extends NameAPI

}

trait Symbols { self: SymTab =>

  class Symbol extends SymbolAPI

}

trait Scopes { self: SymTab =>

  class ScopeEntry extends ScopeEntryAPI
  class Scope {
    def unlink(e: ScopeEntry): Unit = ???
    def unlink(e: Symbol): Unit = ???
  }

}

trait SymTab extends API with Names with Scopes {

}


trait SyncOps extends SymTab {

  trait SyncScope extends Scope {
    override def unlink(e: ScopeEntry): Unit = ???
    override def unlink(e: Symbol): Unit = ???
  }

}
