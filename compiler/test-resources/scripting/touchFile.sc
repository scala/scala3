#!/usr/bin/env scala

import java.io.File

// create an empty file
def main(args: Array[String]): Unit =
  val file = File("touchedFile.out")
  file.createNewFile();
