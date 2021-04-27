package mylib

export numerics._ // error: package numerics is not a valid prefix for a wildcard export...

export defaults.{given Ring[Int]} // error: package defaults is not a valid prefix for a wildcard export...

export enums.enumOrdinal // ok, enums is a package but export is not wildcard
