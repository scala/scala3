//> using options -Wunused:all

import io.circe.Codec

import iron.:|
import iron.circe.given
import iron.constraint.string.StartWith

case class Alien(name: String :| StartWith["alen"]) derives Codec.AsObject

