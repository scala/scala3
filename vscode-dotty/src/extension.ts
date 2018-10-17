'use strict';

import * as fs from 'fs';
import * as path from 'path';

import * as pcp from 'promisify-child-process';
import * as compareVersions from 'compare-versions';

import { ChildProcess } from "child_process";

import { ExtensionContext } from 'vscode';
import * as vscode from 'vscode';
import { LanguageClient, LanguageClientOptions, RevealOutputChannelOn,
         ServerOptions } from 'vscode-languageclient';
import { enableOldServerWorkaround } from './compat'
import { WorksheetPublishOutputNotification } from './protocol'
import * as worksheet from './worksheet'
import * as features from './features'

export let client: LanguageClient

import * as rpc from 'vscode-jsonrpc'
import * as sbtserver from './sbt-server'

let extensionContext: ExtensionContext
let outputChannel: vscode.OutputChannel

/** The sbt process that may have been started by this extension */
let sbtProcess: ChildProcess | undefined

const sbtVersion = "1.2.3"
const sbtArtifact = `org.scala-sbt:sbt-launch:${sbtVersion}`
export const workspaceRoot = `${vscode.workspace.rootPath}`
const disableDottyIDEFile = path.join(workspaceRoot, ".dotty-ide-disabled")
const sbtProjectDir = path.join(workspaceRoot, "project")
const sbtPluginFile = path.join(sbtProjectDir, "dotty-plugin.sbt")
const sbtBuildPropertiesFile = path.join(sbtProjectDir, "build.properties")
const sbtBuildSbtFile = path.join(workspaceRoot, "build.sbt")
const languageServerArtifactFile = path.join(workspaceRoot, ".dotty-ide-artifact")

function isConfiguredProject() {
  return (   fs.existsSync(sbtPluginFile)
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

  } else if (!fs.existsSync(disableDottyIDEFile)) {
    let configuredProject: Thenable<void> = Promise.resolve()
    if (!isConfiguredProject()) {
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
        .then(_ => connectToSbt(coursierPath))
        .then(sbt => {
          return withProgress("Configuring Dotty IDE...", configureIDE(sbt))
           .then(_ => { sbtserver.tellSbt(outputChannel, sbt, "exit") })
        })
    }

    configuredProject
      .then(_ => runLanguageServer(coursierPath, languageServerArtifactFile))
  }
}

/**
 * Connect to sbt server (possibly by starting a new instance) and keep verifying that the
 * connection is still alive. If it dies, restart sbt server.
 */
function connectToSbt(coursierPath: string): Thenable<rpc.MessageConnection> {

  return offeringToRetry(() => {
    return withSbtInstance(coursierPath).then(connection => {
      return connection
    })
  }, "Couldn't connect to sbt server (see log for details)")
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
function configureIDE(sbt: rpc.MessageConnection): Thenable<sbtserver.ExecResult> {

  const tellSbt = (command: string) => {
    return () => sbtserver.tellSbt(outputChannel, sbt, command)
  }

  const failMessage = "`configureIDE` failed (see log for details)"

  // `configureIDE` is a command, which means that upon failure, sbt won't tell us anything
  // until sbt/sbt#4370 is fixed.
  // We run `compile` and `test:compile` first because they're tasks (so we get feedback from sbt
  // in case of failure), and we're pretty sure configureIDE will pass if they passed.
  return offeringToRetry(tellSbt("compile"), failMessage).then(_ => {
    return offeringToRetry(tellSbt("test:compile"), failMessage).then(_ => {
      return offeringToRetry(tellSbt("configureIDE"), failMessage)
    })
  })
}

/**
 * Present the user with a dialog to retry `op` after a failure, returns its result in case of
 * success.
 *
 * @param op The operation to perform
 * @param failMessage The message to display in the dialog offering to retry `op`.
 * @return A promise that will either resolve to the result of `op`, or a dialog that will let
 * the user retry the operation.
 */
function offeringToRetry<T>(op: () => Thenable<T>, failMessage: string): Thenable<T> {
  return op()
    .then(success => Promise.resolve(success),
      _ => {
        outputChannel.show()
        return vscode.window.showErrorMessage(failMessage, "Retry?")
          .then(retry => {
            if (retry) return offeringToRetry(op, failMessage)
            else return Promise.reject()
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

function startNewSbtInstance(coursierPath: string) {
  fetchWithCoursier(coursierPath, sbtArtifact).then((sbtClasspath) => {
    sbtProcess = pcp.spawn("java", [
      "-Dsbt.log.noformat=true",
      "-classpath", sbtClasspath,
      "xsbt.boot.Boot"
    ], {
      cwd: workspaceRoot
    })

    // Close stdin, otherwise in case of error sbt will block waiting for the
    // user input to reload or exit the build.
    sbtProcess.stdin.end()

    sbtProcess.stdout.on('data', data => {
      outputChannel.append(data.toString())
    })
    sbtProcess.stderr.on('data', data => {
      outputChannel.append(data.toString())
    })
  })
}

/**
 * Connects to an existing sbt server, or boots up one instance and connects to it.
 */
function withSbtInstance(coursierPath: string): Thenable<rpc.MessageConnection> {
  const serverSocketInfo = path.join(workspaceRoot, "project", "target", "active.json")

  if (!fs.existsSync(serverSocketInfo)) {
    startNewSbtInstance(coursierPath)
  }

  return sbtserver.connectToSbtServer(outputChannel)
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
      const coursierProc = pcp.spawn("java", args)

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
      return coursierProc.then(() => { return classPath })
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
  client.registerFeature(new features.WorksheetRunFeature(client))

  if (isOldServer)
    enableOldServerWorkaround(client)

  // Push the disposable to the context's subscriptions so that the
  // client can be deactivated on extension deactivation
  extensionContext.subscriptions.push(client.start())
}
