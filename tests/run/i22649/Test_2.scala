@main def Test =
  val str =
    structureOf[{
      type T
      def make: T
    }]

  assert(
    str == """RecursiveType(rec1 => Refinement(Refinement(TypeRef(ThisType(TypeRef(NoPrefix(), "lang")), "Object"), "T", TypeBounds(TypeRef(ThisType(TypeRef(NoPrefix(), "scala")), "Nothing"), TypeRef(ThisType(TypeRef(NoPrefix(), "scala")), "Any"))), "make", ByNameType(TypeRef(RecursiveThis(<rec1>), "T"))))""",
    s"Was $str"
  )
