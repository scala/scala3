package database

object B {
  trait GetValue[T]

  object GetValue {
    implicit def inst[T]: GetValue[T] = ???
  }

  class ResultSet {
    def getV[A: GetValue]: A = ???
  }

  trait DBParse[T]

  class AVG() {
    def call: String = "AVG2"
  }

  object ClientOwnerId {
    class CompanyId

    def parseClientOwnerId[T: DBParse]: Unit = {}
  }

  class Wrapper(companyId: ClientOwnerId.CompanyId)
}
