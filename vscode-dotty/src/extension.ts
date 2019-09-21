import * as fs from 'fs'
import * as path from 'path'

import * as pcp from 'promisify-child-process'
import * as compareVersions from 'compare-versions'

import { ChildProcess } from "child_process"

import { ExtensionContext,  Disposable } from 'vscode'
import * as vscode from 'vscode'
import { LanguageClient, LanguageClientOptions, RevealOutputChannelOn,
         ServerOptions } from 'vscode-languageclient'
import { enableOldServerWorkaround } from './compat'
import * as features from './features'
import { DecompiledDocumentProvider } from './tasty-decompiler'

export let client: LanguageClient

import * as rpc from 'vscode-jsonrpc'
import * as sbtserver from './sbt-server'
import { Tracer } from './tracer'

export const extensionName = 'dotty'
const extensionConfig = vscode.workspace.getConfiguration(extensionName)

let extensionContext: ExtensionContext
let outputChannel: vscode.OutputChannel
let tracer: Tracer

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
const languageServerConfigFile = path.join(workspaceRoot, ".dotty-ide.json")

function isConfiguredProject() {
  return (   fs.existsSync(sbtPluginFile)
          || fs.existsSync(sbtBuildPropertiesFile)
          || fs.existsSync(sbtBuildSbtFile)
          || (fs.existsSync(languageServerArtifactFile) && fs.existsSync(languageServerConfigFile))
  )
}

export function activate(context: ExtensionContext) {
  // Override the language configuration from vscode-scala (doing this from
  // package.json does not work)
  vscode.languages.setLanguageConfiguration("scala", {
    "indentationRules": {
      // Auto-indent when pressing enter on a line matching this regexp, in details:
      // 1. If they're not preceded by `end`, auto-indent after `while`, `for`, `match`, `try`, `if`
      // 2. Auto-indent after `if ...` as long as it doesn't match `if ... then ...`
      // 3. Auto-indent after `then`, `else`, `do`, `catch`, `finally`, `yield`, `case`, `=`, `=>`, `<-`, `=>>`x
      "increaseIndentPattern":
        /(((?<!\bend\b\s*?)\b(if|while|for|match|try))|(\bif\s+(?!.*?\bthen\b.*?$)[^\s]*?)|(\b(then|else|do|catch|finally|yield|case))|=|=>|<-|=>>)\s*?$/,
      // Only auto-unindent completed `end` folowed by `while`, `for`, `match`, `try`, `if`
      "decreaseIndentPattern": /(^\s*end\b\s*)\b(if|while|for|match|try)$/
    }
  })

  extensionContext = context
  outputChannel = vscode.window.createOutputChannel("Dotty")
  tracer = new Tracer({
    extensionContext,
    extensionConfig,
    extensionOut: outputChannel,
  })

  const coursierPath = path.join(extensionContext.extensionPath, "out", "coursier")
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

    if (!vscode.workspace.workspaceFolders) {
      const editor = vscode.window.activeTextEditor
      if (editor && editor.document.uri.fsPath && editor.document.uri.fsPath.length > 0) {
        setWorkspaceAndReload(editor.document)
      }
    } else {
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
}

/**
 * Find and set a workspace root if no folders are open in the workspace. If there are already
 * folders open in the workspace, do nothing.
 *
 * Adding a first folder to the workspace completely reloads the extension.
 */
function setWorkspaceAndReload(document: vscode.TextDocument) {
  const documentPath = path.parse(document.uri.fsPath).dir
  const workspaceRoot = findWorkspaceRoot(documentPath) || documentPath

  vscode.window.showInformationMessage(
    `It looks like '${workspaceRoot}' is the root of your Scala workspace. ` +
    'Would you like to open it?',
    'Yes', 'No'
  ).then((value: String | undefined) => {
    if (value === 'Yes') {
      vscode.workspace.updateWorkspaceFolders(0, null, { uri: vscode.Uri.file(workspaceRoot) })
    }
  })
}

/**
 * Find the closest parent of `current` that contains a `build.sbt`.
 */
function findWorkspaceRoot(current: string): string | undefined {
  const build = path.join(current, "build.sbt")
  if (fs.existsSync(build)) return current
  else {
    const parent = path.resolve(current, "..")
    if (parent != current) {
      return findWorkspaceRoot(parent)
    }
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
  const lspOutputChannel = tracer.run()

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
    outputChannel: lspOutputChannel,
    revealOutputChannelOn: RevealOutputChannelOn.Never
  }

  // register DecompiledDocumentProvider for Tasty decompiler results
  const provider = new DecompiledDocumentProvider()

  const providerRegistration = Disposable.from(
    vscode.workspace.registerTextDocumentContentProvider(DecompiledDocumentProvider.scheme, provider)
  )

  extensionContext.subscriptions.push(providerRegistration, provider)

  client = new LanguageClient(extensionName, "Dotty", serverOptions, clientOptions)
  client.registerFeature(new features.WorksheetRunFeature(client))
  client.registerFeature(new features.TastyDecompilerFeature(client, provider))

  if (isOldServer)
    enableOldServerWorkaround(client)

  // Push the disposable to the context's subscriptions so that the
  // client can be deactivated on extension deactivation
  extensionContext.subscriptions.push(client.start())
}
