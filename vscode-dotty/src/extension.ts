'use strict';

import * as fs from 'fs';
import * as path from 'path';

import * as cpp from 'child-process-promise';

import { ExtensionContext } from 'vscode';
import * as vscode from 'vscode';
import { LanguageClient, LanguageClientOptions, RevealOutputChannelOn,
         ServerOptions } from 'vscode-languageclient';

let extensionContext: ExtensionContext
let outputChannel: vscode.OutputChannel

export function activate(context: ExtensionContext) {
  extensionContext = context
  outputChannel = vscode.window.createOutputChannel('Dotty Language Client');

  const artifactFile = `${vscode.workspace.rootPath}/.dotty-ide-artifact`
  fs.readFile(artifactFile, (err, data) => {
    if (err) {
      outputChannel.append(`Unable to parse ${artifactFile}`)
      throw err
    }
    const artifact = data.toString().trim()

    if (process.env['DLS_DEV_MODE']) {
      const portFile = `${vscode.workspace.rootPath}/.dotty-ide-dev-port`
      fs.readFile(portFile, (err, port) => {
        if (err) {
          outputChannel.append(`Unable to parse ${portFile}`)
          throw err
        }

        run({
          module: context.asAbsolutePath('out/src/passthrough-server.js'),
          args: [ port.toString() ]
        })
      })
    } else {
      fetchAndRun(artifact)
    }
  })
}

function fetchAndRun(artifact: string) {
  const coursierPath = path.join(extensionContext.extensionPath, './out/coursier');

  vscode.window.withProgress({
    location: vscode.ProgressLocation.Window,
    title: 'Fetching the Dotty Language Server'
  }, (progress) => {

    const coursierPromise =
      cpp.spawn("java", [
        "-jar", coursierPath,
        "fetch",
        "-p",
        artifact
      ])
    const coursierProc = coursierPromise.childProcess

    let classPath = ""

    coursierProc.stdout.on('data', (data: Buffer) => {
      classPath += data.toString().trim()
    })
    coursierProc.stderr.on('data', (data: Buffer) => {
      let msg = data.toString()
      outputChannel.append(msg)
    })

    coursierProc.on('close', (code: number) => {
      if (code != 0) {
        let msg = "Fetching the language server failed."
        outputChannel.append(msg)
        throw new Error(msg)
      }

      run({
        command: "java",
        args: ["-classpath", classPath, "dotty.tools.languageserver.Main", "-stdio"]
      })
    })
    return coursierPromise
  })
}

function run(serverOptions: ServerOptions) {
  const clientOptions: LanguageClientOptions = {
    documentSelector: [
      { language: 'scala', scheme: 'file', pattern: '**/*.scala' },
      { language: 'scala', scheme: 'untitled', pattern: '**/*.scala' }
    ],
    synchronize: {
      configurationSection: 'dotty'
    },
    revealOutputChannelOn: RevealOutputChannelOn.Never
  }

  outputChannel.dispose()

  const client = new LanguageClient('dotty', 'Dotty Language Server', serverOptions, clientOptions);

  // Push the disposable to the context's subscriptions so that the
  // client can be deactivated on extension deactivation
  extensionContext.subscriptions.push(client.start());
}
