package scala {
  package meta {
    package config {
      case class Version()

      trait Aliases {
        type Version = scala.meta.config.Version
        val Version = scala.meta.config.Version
      }
    }
  }

  package object meta extends scala.meta.config.Aliases
}
