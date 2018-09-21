'use strict';

import * as fs from 'fs';
import * as path from 'path';

import * as cpp from 'child-process-promise';
import * as compareVersions from 'compare-versions';

import { ChildProcess } from "child_process";

import { ExtensionContext } from 'vscode';
import * as vscode from 'vscode';
import { LanguageClient, LanguageClientOptions, RevealOutputChannelOn,
         ServerOptions } from 'vscode-languageclient';
import { enableOldServerWorkaround } from './compat'

import * as worksheet from './worksheet'

import * as rpc from 'vscode-jsonrpc'
import * as sbtserver from './sbt-server'

let extensionContext: ExtensionContext
let outputChannel: vscode.OutputChannel
export let client: LanguageClient

/** The sbt process that may have been started by this extension */
let sbtProcess: ChildProcess

const sbtVersion = "1.2.3"
const sbtArtifact = `org.scala-sbt:sbt-launch:${sbtVersion}`
const workspaceRoot = `${vscode.workspace.rootPath}`
const disableDottyIDEFile = path.join(workspaceRoot, ".dotty-ide-disabled")
const sbtProjectDir = path.join(workspaceRoot, "project")
const sbtPluginFile = path.join(sbtProjectDir, "dotty-plugin.sbt")
const sbtBuildPropertiesFile = path.join(sbtProjectDir, "build.properties")
const sbtBuildSbtFile = path.join(workspaceRoot, "build.sbt")
const languageServerArtifactFile = path.join(workspaceRoot, ".dotty-ide-artifact")

function isUnconfiguredProject() {
  return !(   fs.existsSync(disableDottyIDEFile)
           || fs.existsSync(sbtPluginFile)
           || fs.existsSync(sbtBuildPropertiesFile)
           || fs.existsSync(sbtBuildSbtFile)
  )
}

export function activate(context: ExtensionContext) {
  extensionContext = context
  outputChannel = vscode.window.createOutputChannel("Dotty");

  const coursierPath = path.join(extensionContext.extensionPath, "out", "coursier");
  const dottyPluginSbtFileSource = path.join(extensionContext.extensionPath, "out", "dotty-plugin.sbt")
  const buildSbtFileSource = path.join(extensionContext.extensionPath, "out", "build.sbt")

  vscode.workspace.onWillSaveTextDocument(worksheet.prepareWorksheet)
  vscode.workspace.onDidSaveTextDocument(document => {
    if (worksheet.isWorksheet(document)) {
      worksheet.evaluateWorksheet(document)
    }
  })
  vscode.workspace.onDidCloseTextDocument(document => {
    if (worksheet.isWorksheet(document)) {
      worksheet.removeWorksheet(document)
    }
  })

  if (process.env['DLS_DEV_MODE']) {
    const portFile = path.join(workspaceRoot, ".dotty-ide-dev-port")
    fs.readFile(portFile, (err, port) => {
      if (err) {
        outputChannel.appendLine(`Unable to parse ${portFile}`)
        throw err
      }

      run({
        module: context.asAbsolutePath(path.join("out", "src", "passthrough-server.js")),
        args: [ port.toString() ]
      }, false)
    })

  } else {
    let configuredProject: Thenable<void> = Promise.resolve()
    if (isUnconfiguredProject()) {
      configuredProject = vscode.window.showInformationMessage(
        "This looks like an unconfigured Scala project. Would you like to start the Dotty IDE?",
        "Yes", "No"
      ).then(choice => {
        if (choice === "Yes") {
          bootstrapSbtProject(buildSbtFileSource, dottyPluginSbtFileSource)
          return Promise.resolve()
        } else if (choice === "No") {
          fs.appendFile(disableDottyIDEFile, "", _ => {})
          return Promise.reject()
        }
      })
    }

    configuredProject
      .then(_ => withProgress("Configuring Dotty IDE...", configureIDE(coursierPath)))
      .then(_ => runLanguageServer(coursierPath, languageServerArtifactFile))
  }
}

export function deactivate() {
  // If sbt was started by this extension, kill the process.
  // FIXME: This will be a problem for other clients of this server.
  if (sbtProcess) {
    sbtProcess.kill()
  }
}

/**
 * Display a progress bar with title `title` while `op` completes.
 *
 * @param title The title of the progress bar
 * @param op The thenable that is monitored by the progress bar.
 */
function withProgress<T>(title: string, op: Thenable<T>): Thenable<T> {
  return vscode.window.withProgress({
    location: vscode.ProgressLocation.Window,
    title: title
  }, _ => op)
}

/** Connect to an sbt server and run `configureIDE`. */
function configureIDE(coursierPath: string): Thenable<sbtserver.ExecResult> {

  function offeringToRetry(client: rpc.MessageConnection, command: string): Thenable<sbtserver.ExecResult> {
    return sbtserver.tellSbt(outputChannel, client, command)
      .then(success => Promise.resolve(success),
        _ => {
          outputChannel.show()
          return vscode.window.showErrorMessage("IDE configuration failed (see logs for details)", "Retry?")
            .then(retry => {
              if (retry) return offeringToRetry(client, command)
              else return Promise.reject()
            })
        })
  }

  return withSbtInstance(outputChannel, coursierPath)
    .then(client => {
      // `configureIDE` is a command, which means that upon failure, sbt won't tell us anything
      // until sbt/sbt#4370 is fixed.
      // We run `compile` and `test:compile` first because they're tasks (so we get feedback from sbt
      // in case of failure), and we're pretty sure configureIDE will pass if they passed.
      return offeringToRetry(client, "compile").then(_ => {
        return offeringToRetry(client, "test:compile").then(_ => {
          return offeringToRetry(client, "configureIDE")
        })
      })
    })
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

/**
 * Connects to an existing sbt server, or boots up one instance and connects to it.
 */
function withSbtInstance(log: vscode.OutputChannel, coursierPath: string): Thenable<rpc.MessageConnection> {
  const serverSocketInfo = path.join(workspaceRoot, "project", "target", "active.json")

  if (!fs.existsSync(serverSocketInfo)) {
    fetchWithCoursier(coursierPath, sbtArtifact).then((sbtClasspath) => {
      sbtProcess = cpp.spawn("java", [
        "-Dsbt.log.noformat=true",
        "-classpath", sbtClasspath,
        "xsbt.boot.Boot"
      ]).childProcess

      // Close stdin, otherwise in case of error sbt will block waiting for the
      // user input to reload or exit the build.
      sbtProcess.stdin.end()

      sbtProcess.stdout.on('data', data => {
        log.appendLine(data.toString())
      })
      sbtProcess.stderr.on('data', data => {
        log.appendLine(data.toString())
      })
    })
  }

  return sbtserver.connectToSbtServer(log)
}

function fetchWithCoursier(coursierPath: string, artifact: string, extra: string[] = []) {
  return vscode.window.withProgress({
      location: vscode.ProgressLocation.Window,
      title: `Fetching ${ artifact }`
    }, _ => {
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

function bootstrapSbtProject(buildSbtFileSource: string,
                             dottyPluginSbtFileSource: string) {
    fs.mkdirSync(sbtProjectDir)
    fs.appendFileSync(sbtBuildPropertiesFile, `sbt.version=${sbtVersion}`)
    fs.copyFileSync(buildSbtFileSource, sbtBuildSbtFile)
    fs.copyFileSync(dottyPluginSbtFileSource, path.join(sbtProjectDir, "plugins.sbt"))
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

  client.onReady().then(() => {
    client.onNotification("worksheet/publishOutput", (params) => {
      worksheet.handleMessage(params)
    })
  })

  vscode.commands.registerCommand(worksheet.worksheetEvaluateKey, () => {
    worksheet.evaluateWorksheetCommand()
  })

  // Push the disposable to the context's subscriptions so that the
  // client can be deactivated on extension deactivation
  extensionContext.subscriptions.push(client.start());
}
