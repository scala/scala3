package tests

package implicitMembers

class OuterClass:

  class ImplicitMemberTarget
  object ImplicitMemberTarget:
    extension (a: ImplicitMemberTarget) def extensionFromCompanion(i: Int): String//expected: def extensionFromCompanion(i: Int): String
      = "ImplicitMemberTarget"
  extension (a: ImplicitMemberTarget) def extensionFromOuterClass(i: Int): String//expected: def extensionFromOuterClass(i: Int): String
   = "ImplicitMemberTarget"

extension (a: OuterClass#ImplicitMemberTarget) def extensionFromPackage(i: Int): String//expected: def extensionFromPackage(i: Int): String
  =  "ImplicitMemberTarget"
