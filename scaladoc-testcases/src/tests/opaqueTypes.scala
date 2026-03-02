package tests

package opaqueTypes

opaque type Permissions
 = Int
opaque type PermissionChoice
 = Int
//opaque type Permission <: Permissions & PermissionChoice = Int TODO: #112

object Foo:
  opaque type Bar
   = Int