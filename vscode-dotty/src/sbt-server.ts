/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */
// Copy pasted from vscode-sbt-scala

'use strict'

import * as fs from 'fs'
import * as net from 'net'
import * as os from 'os'
import * as path from 'path'
import * as url from 'url'

import * as rpc from 'vscode-jsonrpc'

import * as vscode from 'vscode'

import { workspaceRoot } from './extension'

/** The result of successful `sbt/exec` call. */
export interface ExecResult {
  status: string
  channelName: string
  execId: number
  commandQueue: string[]
  exitCode: number
}

class CommandLine {
  commandLine: string
  constructor(commandLine: string) {
    this.commandLine = commandLine
  }
}

/**
 * Sends `command` to sbt with `sbt/exec`.
 *
 * @param log Where to log messages between this client and sbt server
 * @param connection The connection to sbt server to use
 * @param command The command to send to sbt
 *
 * @return The result of executing `command`.
 */
export async function tellSbt(log: vscode.OutputChannel,
                              connection: rpc.MessageConnection,
                              command: string): Promise<ExecResult> {
  log.appendLine(`>>> ${command}`)
  const req = new rpc.RequestType<CommandLine, ExecResult, any, any>("sbt/exec")
  return await connection.sendRequest(req, new CommandLine(command))
}

/**
 * Attempts to connect to an sbt server running in this workspace.
 *
 * @param log Where to log messages between VSCode and sbt server.
 */
export function connectToSbtServer(log: vscode.OutputChannel): Promise<rpc.MessageConnection> {
  return waitForServer().then(socket => {
    let connection = rpc.createMessageConnection(
      new rpc.StreamMessageReader(socket),
      new rpc.StreamMessageWriter(socket))

    connection.listen()

    connection.onNotification("window/logMessage", (params) => {
      log.appendLine(`<<< [${messageTypeToString(params.type)}] ${params.message}`)
    })

    return connection
  })
}

function connectSocket(socket: net.Socket): net.Socket {
  let u = discoverUrl();
  if (u.protocol == 'tcp:' && u.port) {
    socket.connect(+u.port, '127.0.0.1');
  } else if (u.protocol == 'local:' && u.hostname && os.platform() == 'win32') {
    let pipePath = '\\\\.\\pipe\\' + u.hostname
    socket.connect(pipePath)
  } else if (u.protocol == 'local:' && u.path) {
    socket.connect(u.path)
  } else {
    throw 'Unknown protocol ' + u.protocol
  }
  return socket
}

// the port file is hardcoded to a particular location relative to the build.
function discoverUrl(): url.Url {
  let pf = path.join(workspaceRoot, 'project', 'target', 'active.json')
  let portfile = JSON.parse(fs.readFileSync(pf).toString())
  return url.parse(portfile.uri)
}

function delay(ms: number) {
	return new Promise(resolve => setTimeout(resolve, ms))
}

async function waitForServer(): Promise<net.Socket> {
  let socket: net.Socket
  return vscode.window.withProgress({
    location: vscode.ProgressLocation.Window,
    title: "Connecting to sbt server..."
  }, async _ => {
    let retries = 60
    while (!socket && retries > 0) {
      try { socket = connectSocket(new net.Socket()) }
      catch (e) {
        retries--
        await delay(1000)
      }
    }
    if (socket) return Promise.resolve(socket)
    else return Promise.reject()
  })
}

function messageTypeToString(messageType: number): string {
  if (messageType == 1) return "error"
  else if (messageType == 2) return "warn"
  else if (messageType == 3) return "info"
  else if (messageType == 4) return "log"
  else return "???"
}

