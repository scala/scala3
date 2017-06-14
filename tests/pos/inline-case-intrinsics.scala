trait Trait { self =>
  private case class CC(name: String)

  def m = {
    // The type of this Apply after .widenDealias is an AndType:
    // TypeRef(AndType(
    //   TypeRef(ThisType(TypeRef(NoPrefix,<empty>)),Trait),
    //   ThisType(TypeRef(ThisType(TypeRef(NoPrefix,<empty>)),Trait))
    // ), CC)
    // Which cannot be used as is in a New....

    val a = CC("")
    println(a)
    ()
  }
}
