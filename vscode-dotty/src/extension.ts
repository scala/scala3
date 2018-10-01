'use strict';

import * as fs from 'fs';
import * as path from 'path';

import * as cpp from 'child-process-promise';
import * as compareVersions from 'compare-versions';

import { ExtensionContext } from 'vscode';
import * as vscode from 'vscode';
import { LanguageClient, LanguageClientOptions, RevealOutputChannelOn,
         ServerOptions } from 'vscode-languageclient';
import { enableOldServerWorkaround } from './compat'

import * as worksheet from './worksheet'

let extensionContext: ExtensionContext
let outputChannel: vscode.OutputChannel
let client: LanguageClient

export function activate(context: ExtensionContext) {
  extensionContext = context
  outputChannel = vscode.window.createOutputChannel("Dotty");

  const sbtArtifact = "org.scala-sbt:sbt-launch:1.2.3"
  const buildSbtFile = `${vscode.workspace.rootPath}/build.sbt`
  const dottyPluginSbtFile = path.join(extensionContext.extensionPath, './out/dotty-plugin.sbt')
  const disableDottyIDEFile = `${vscode.workspace.rootPath}/.dotty-ide-disabled`
  const languageServerArtifactFile = `${vscode.workspace.rootPath}/.dotty-ide-artifact`
  const languageServerDefaultConfigFile = path.join(extensionContext.extensionPath, './out/default-dotty-ide-config')
  const coursierPath = path.join(extensionContext.extensionPath, './out/coursier');

  vscode.workspace.onWillSaveTextDocument(worksheet.prepareWorksheet)
  vscode.workspace.onDidSaveTextDocument(document => {
    if (worksheet.isWorksheet(document)) {
      vscode.commands.executeCommand(worksheet.worksheetEvaluateAfterSaveKey)
    }
  })
  vscode.workspace.onDidCloseTextDocument(document => {
    if (worksheet.isWorksheet(document)) {
      worksheet.removeWorksheet(document)
    }
  })

  vscode.commands.registerCommand(worksheet.worksheetEvaluateAfterSaveKey, () => {
    worksheet.evaluateCommand()
  })

  if (process.env['DLS_DEV_MODE']) {
    const portFile = `${vscode.workspace.rootPath}/.dotty-ide-dev-port`
    fs.readFile(portFile, (err, port) => {
      if (err) {
        outputChannel.appendLine(`Unable to parse ${portFile}`)
        throw err
      }

      run({
        module: context.asAbsolutePath('out/src/passthrough-server.js'),
        args: [ port.toString() ]
      }, false)
    })

  } else {
    // Check whether `.dotty-ide-artifact` exists. If it does, start the language server,
    // otherwise, try propose to start it if there's no build.sbt
    if (fs.existsSync(languageServerArtifactFile)) {
      runLanguageServer(coursierPath, languageServerArtifactFile)
    } else if (!fs.existsSync(disableDottyIDEFile) && !fs.existsSync(buildSbtFile)) {
      vscode.window.showInformationMessage(
          "This looks like an unconfigured Scala project. Would you like to start the Dotty IDE?",
          "Yes", "No"
      ).then(choice => {
        if (choice == "Yes") {
          fs.readFile(languageServerDefaultConfigFile, (err, data) => {
            if (err) throw err
            else {
              const languageServerScalaVersion = data.toString().trim()
              fetchAndConfigure(coursierPath, sbtArtifact, languageServerScalaVersion, dottyPluginSbtFile).then(() => {
                runLanguageServer(coursierPath, languageServerArtifactFile)
              })
            }
          })
        } else {
          fs.appendFile(disableDottyIDEFile, "", _ => {})
        }
      })
    }
  }
}

function runLanguageServer(coursierPath: string, languageServerArtifactFile: string) {
  fs.readFile(languageServerArtifactFile, (err, data) => {
    if (err) throw err
    else {
      const languageServerArtifact = data.toString().trim()
      const languageServerVersion = languageServerArtifact.split(":")[2]
      const isOldServer = compareVersions(languageServerVersion, "0.9.x") <= 0
      fetchWithCoursier(coursierPath, languageServerArtifact).then((languageServerClasspath) => {
        run({
          command: "java",
          args: ["-classpath", languageServerClasspath, "dotty.tools.languageserver.Main", "-stdio"]
        }, isOldServer)
      })
    }
  })
}

function fetchAndConfigure(coursierPath: string, sbtArtifact: string, languageServerScalaVersion: string, dottyPluginSbtFile: string) {
    return fetchWithCoursier(coursierPath, sbtArtifact).then((sbtClasspath) => {
        return configureIDE(sbtClasspath, languageServerScalaVersion, dottyPluginSbtFile)
    })
}

function fetchWithCoursier(coursierPath: string, artifact: string, extra: string[] = []) {
  return vscode.window.withProgress({
      location: vscode.ProgressLocation.Window,
      title: `Fetching ${ artifact }`
    }, (progress) => {
      const args = [
        "-jar", coursierPath,
        "fetch",
        "-p",
        artifact
      ].concat(extra)
      const coursierPromise = cpp.spawn("java", args)
      const coursierProc = coursierPromise.childProcess

      let classPath = ""

      coursierProc.stdout.on('data', (data: Buffer) => {
        classPath += data.toString().trim()
      })
      coursierProc.stderr.on('data', (data: Buffer) => {
        let msg = data.toString().trim()
        outputChannel.appendLine(msg)
      })

      coursierProc.on('close', (code: number) => {
        if (code != 0) {
          let msg = `Couldn't fetch '${ artifact }' (exit code ${ code }).`
          outputChannel.appendLine(msg)
          throw new Error(msg)
        }
      })
      return coursierPromise.then(() => { return classPath })
    })
}

function configureIDE(sbtClasspath: string, languageServerScalaVersion: string, dottyPluginSbtFile: string) {
  return vscode.window.withProgress({
    location: vscode.ProgressLocation.Window,
    title: 'Configuring the IDE for Dotty...'
  }, (progress) => {

    // Run sbt to configure the IDE. If the `DottyPlugin` is not present, dynamically load it and
    // eventually run `configureIDE`.
    const sbtPromise =
      cpp.spawn("java", [
        "-Dsbt.log.noformat=true",
        "-classpath", sbtClasspath,
        "xsbt.boot.Boot",
        `--addPluginSbtFile=${dottyPluginSbtFile}`,
        `set every scalaVersion := "${languageServerScalaVersion}"`,
        "configureIDE"
      ])

    const sbtProc = sbtPromise.childProcess
    // Close stdin, otherwise in case of error sbt will block waiting for the
    // user input to reload or exit the build.
    sbtProc.stdin.end()

    sbtProc.stdout.on('data', (data: Buffer) => {
      let msg = data.toString().trim()
      outputChannel.appendLine(msg)
    })
    sbtProc.stderr.on('data', (data: Buffer) => {
      let msg = data.toString().trim()
      outputChannel.appendLine(msg)
    })

    sbtProc.on('close', (code: number) => {
      if (code != 0) {
        const msg = "Configuring the IDE failed."
        outputChannel.appendLine(msg)
        throw new Error(msg)
      }
    })

      return sbtPromise
  })
}

function run(serverOptions: ServerOptions, isOldServer: boolean) {
  const clientOptions: LanguageClientOptions = {
    documentSelector: [
      { scheme: 'file', pattern: '**/*.sc' },
      { scheme: 'untitled', pattern: '**/*.sc' },
      { scheme: 'file', pattern: '**/*.scala' },
      { scheme: 'untitled', pattern: '**/*.scala' }
    ],
    synchronize: {
      configurationSection: 'dotty'
    },
    outputChannel: outputChannel,
    revealOutputChannelOn: RevealOutputChannelOn.Never
  }

  client = new LanguageClient("dotty", "Dotty", serverOptions, clientOptions)
  if (isOldServer)
    enableOldServerWorkaround(client)

  // We use the `window/logMessage` command to communicate back the result of evaluating
  // a worksheet.
  client.onReady().then(() => {
    client.onNotification("window/logMessage", (params) => {
      worksheet.worksheetHandleMessage(params.message)
    })
  })

  vscode.commands.registerCommand(worksheet.worksheetEvaluateKey, () => {
    worksheet.worksheetSave(client)
  })

  vscode.commands.registerCommand(worksheet.worksheetCancelEvaluationKey, () => {
    worksheet.cancelExecution(client)
  })

  // Push the disposable to the context's subscriptions so that the
  // client can be deactivated on extension deactivation
  extensionContext.subscriptions.push(client.start());
}
