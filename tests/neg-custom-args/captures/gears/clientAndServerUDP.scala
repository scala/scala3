package PosixLikeIO.examples

import gears.async.AsyncOperations.*
import gears.async.default.given
import gears.async.{Async, Future}

import java.net.DatagramPacket
import java.nio.ByteBuffer
import java.nio.file.StandardOpenOption
import scala.concurrent.ExecutionContext

import PosixLikeIO.{PIOHelper, SocketUDP}

@main def clientAndServerUDP(): Unit =
  given ExecutionContext = ExecutionContext.global
  Async.blocking:
    val server = Future:
      PIOHelper.withSocketUDP(8134): serverSocket =>
        val got: DatagramPacket = serverSocket.receive().awaitResult.get
        val messageReceived = String(got.getData.slice(0, got.getLength), "UTF-8")
        val responseMessage = (messageReceived.toInt + 1).toString.getBytes
        serverSocket.send(ByteBuffer.wrap(responseMessage), got.getAddress.toString.substring(1), got.getPort)
        sleep(50)

    def client(value: Int): Future[Unit] =
      Future:
        PIOHelper.withSocketUDP(): clientSocket =>
          val data: Array[Byte] = value.toString.getBytes
          clientSocket.send(ByteBuffer.wrap(data), "localhost", 8134).awaitResult.get
          val responseDatagram = clientSocket.receive().awaitResult.get
          val messageReceived = String(responseDatagram.getData.slice(0, responseDatagram.getLength), "UTF-8").toInt
          println("Sent " + value.toString + " and got " + messageReceived.toString + " in return.")

    client(100).await
    server.await
