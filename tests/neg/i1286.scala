import scala.idontexist // error
import scala.io.Idontexist // error

import scala.io
import io.Idontexist2 // error

import scala.io.{ AnsiColor, Idontexist3 } // error

import scala.io.{ Idontexist4 as Foo } // error
import scala.io.{ Idontexist5 as _ } // error

import scala.language.dynamics
import scala.language.noAutoTupling
import scala.language.idontexist // error

object Test
