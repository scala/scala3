package tests

package implicitMembers

class OuterClass:
  class ImplicitMemberTarget

  object ImplicitMemberTarget:
    extension (a: ImplicitMemberTarget)
      def extensionFromCompanion: String =
        "ImplicitMemberTarget"

  // does not work
  extension (a: ImplicitMemberTarget)
    def extensionFromOuterClass: String =
      "ImplicitMemberTarget"

extension (a: OuterClass#ImplicitMemberTarget)
    def extensionFromPackage: String =
      "ImplicitMemberTarget"