'use strict';

import * as fs from 'fs';
import * as path from 'path';

import * as cpp from 'child-process-promise';

import { ExtensionContext } from 'vscode';
import * as vscode from 'vscode';
import { LanguageClient, LanguageClientOptions, ServerOptions } from 'vscode-languageclient';

let extensionContext: ExtensionContext
let outputChannel: vscode.OutputChannel

export function activate(context: ExtensionContext) {
  extensionContext = context
  outputChannel = vscode.window.createOutputChannel('Dotty Language Client');

  const artifactFile = `${vscode.workspace.rootPath}/.dotty-ide-artifact`
  const defaultArtifact = "ch.epfl.lamp:dotty-language-server_0.8:0.8.0-bin-SNAPSHOT"
  fs.readFile(artifactFile, (err, data) => {
    const artifact = err ? defaultArtifact : data.toString().trim()

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

      configureIDE().then((res) => {
        run({
          command: "java",
          args: ["-classpath", classPath, "dotty.tools.languageserver.Main", "-stdio"]
        })
      })
    })
    return coursierPromise
  })
}

function configureIDE() {
  const coursierPath = path.join(extensionContext.extensionPath, './out/coursier');
  const loadPluginPath = path.join(extensionContext.extensionPath, './out/load-plugin.jar');

  return vscode.window.withProgress({
    location: vscode.ProgressLocation.Window,
    title: 'Configuring IDE...'
  }, (progress) => {

      const applyLoadPlugin = "apply -cp " + loadPluginPath + " ch.epfl.scala.loadplugin.LoadPlugin"
      const ifAbsentCommands = [
        "if-absent dotty.tools.sbtplugin.DottyPlugin",
        "\"set every scalaVersion := \\\"0.8.0-bin-SNAPSHOT\\\"\"",
        "\"load-plugin ch.epfl.lamp:sbt-dotty:0.2.0-SNAPSHOT dotty.tools.sbtplugin.DottyPlugin\"",
        "\"load-plugin ch.epfl.lamp:sbt-dotty:0.2.0-SNAPSHOT dotty.tools.sbtplugin.DottyIDEPlugin\""
      ].join(" ")

    const sbtPromise =
      cpp.spawn("java", [
          "-jar", coursierPath,
          "launch",
          "org.scala-sbt:sbt-launch:1.1.2", "--",
          applyLoadPlugin,
          ifAbsentCommands,
          "configureIDE"
      ])
    const sbtProc = sbtPromise.childProcess

    sbtProc.on('close', (code: number) => {
      if (code != 0) {
        let msg = "Configuring the IDE failed."
        outputChannel.append(msg)
        throw new Error(msg)
      }
    })

    return sbtPromise;
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
    }
  }

  outputChannel.dispose()

  const client = new LanguageClient('dotty', 'Dotty Language Server', serverOptions, clientOptions);

  // Push the disposable to the context's subscriptions so that the
  // client can be deactivated on extension deactivation
  extensionContext.subscriptions.push(client.start());
}
