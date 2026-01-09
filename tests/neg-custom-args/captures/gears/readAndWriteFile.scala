package PosixLikeIO.examples

import gears.async.Async
import gears.async.default.given

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.nio.file.StandardOpenOption
import scala.concurrent.ExecutionContext

import PosixLikeIO.PIOHelper

@main def readAndWriteFile(): Unit =
  given ExecutionContext = ExecutionContext.global
  Async.blocking:
    PIOHelper.withFile("/home/julian/Desktop/x.txt", StandardOpenOption.READ, StandardOpenOption.WRITE): f =>
      f.writeString("Hello world! (1)").await
      println(f.readString(1024).await)
      f.writeString("Hello world! (2)").await
      println(f.readString(1024).await)
