'use strict';

import {
	IPCMessageReader, IPCMessageWriter,
	createConnection, IConnection, TextDocumentSyncKind,
	TextDocuments, TextDocument, Diagnostic, DiagnosticSeverity,
	InitializeParams, InitializeResult, TextDocumentPositionParams,
	CompletionItem, CompletionItemKind
} from 'vscode-languageserver';

import * as net from 'net';

let argv = process.argv.slice(2)
let port = argv.shift()

let client = new net.Socket()
client.setEncoding('utf8')
process.stdout.setEncoding('utf8')
process.stdin.setEncoding('utf8')

let isConnected = false

client.on('data', (data) => {
  process.stdout.write(data.toString())
})
process.stdin.on('readable', () => {
  let chunk = process.stdin.read();
  if (chunk !== null) {
    if (isConnected) {
      client.write(chunk)
    } else {
      client.on('connect', () => {
        client.write(chunk)
      })
    }
  }
})

client.on('error', (err) => {
  if (!isConnected) {
    startConnection()
  }
})

function startConnection() {
  setTimeout(() => {
    client.connect(port, () => {
      isConnected = true
    })
  }, 1000)
}

startConnection()
