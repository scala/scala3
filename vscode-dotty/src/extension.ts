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

  const sbtArtifact = "org.scala-sbt:sbt-launch:1.1.5"
  const loadPluginArtifact = "ch.epfl.scala:load-plugin_2.12:0.1.0+2-496ac670"
  const buildSbtFile = `${vscode.workspace.rootPath}/build.sbt`
  const languageServerArtifactFile = `${vscode.workspace.rootPath}/.dotty-ide-artifact`
  const languageServerDefaultConfigFile = path.join(extensionContext.extensionPath, './out/default-dotty-ide-config')
  const coursierPath = path.join(extensionContext.extensionPath, './out/coursier');

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
    // Check whether `.dotty-ide-artifact` exists. If it does, start the language server,
    // otherwise, try propose to start it if there's no build.sbt
    if (fs.existsSync(languageServerArtifactFile)) {
      runLanguageServer(coursierPath, languageServerArtifactFile)
    } else if (!fs.existsSync(buildSbtFile)) {
      vscode.window.showInformationMessage(
          "This looks like an unconfigured project. Would you like to start Dotty IDE?",
          "Yes", "No"
      ).then(choice => {
        if (choice == "Yes") {
          fs.readFile(languageServerDefaultConfigFile, (err, data) => {
            if (err) throw err
            else {
              const [languageServerScalaVersion, sbtDottyVersion] = data.toString().trim().split(/\r?\n/)
              fetchAndConfigure(coursierPath, sbtArtifact, languageServerScalaVersion, sbtDottyVersion, loadPluginArtifact).then(() => {
                runLanguageServer(coursierPath, languageServerArtifactFile)
              })
            }
          })
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
      fetchWithCoursier(coursierPath, languageServerArtifact).then((languageServerClasspath) => {
        run({
          command: "java",
          args: ["-classpath", languageServerClasspath, "dotty.tools.languageserver.Main", "-stdio"]
        })
      })
    }
  })
}

function fetchAndConfigure(coursierPath: string, sbtArtifact: string, languageServerScalaVersion: string, sbtDottyVersion: string, loadPluginArtifact: string) {
  const sbtPromise = fetchWithCoursier(coursierPath, sbtArtifact)
  const loadPluginPromise = fetchWithCoursier(coursierPath, loadPluginArtifact)

  return Promise.all([sbtPromise, loadPluginPromise]).then((results) => {
    const [sbtClasspath, loadPluginJar] = results
    return configureIDE(sbtClasspath, languageServerScalaVersion, sbtDottyVersion, loadPluginJar)
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
        outputChannel.append(msg)
      })

      coursierProc.on('close', (code: number) => {
        if (code != 0) {
          let msg = `Couldn't fetch '${ artifact }' (exit code ${ code }).`
          outputChannel.append(msg)
          throw new Error(msg)
        }
      })
      return coursierPromise.then(() => { return classPath })
    })
}

function configureIDE(sbtClasspath: string, languageServerScalaVersion: string, sbtDottyVersion: string, loadPluginJar: string) {
  return vscode.window.withProgress({
    location: vscode.ProgressLocation.Window,
    title: 'Configuring IDE...'
  }, (progress) => {
    const applyLoadPlugin = `apply -cp ${ loadPluginJar } ch.epfl.scala.loadplugin.LoadPlugin`
    const ifAbsentCommands = [
      "if-absent dotty.tools.sbtplugin.DottyPlugin",
      "\"set every scalaVersion := \\\"" + languageServerScalaVersion + "\\\"\"",
      "\"load-plugin ch.epfl.lamp:sbt-dotty:" + sbtDottyVersion + " dotty.tools.sbtplugin.DottyPlugin\"",
      "\"load-plugin ch.epfl.lamp:sbt-dotty:" + sbtDottyVersion + " dotty.tools.sbtplugin.DottyIDEPlugin\""
    ].join(" ")

    // Run sbt to configure the IDE. If the `DottyPlugin` is not present, dynamically load it and
    // eventually run `configureIDE`.
    const sbtPromise =
      cpp.spawn("java", [
        "-classpath", sbtClasspath,
        "xsbt.boot.Boot",
        applyLoadPlugin,
        ifAbsentCommands,
        "configureIDE"
      ])

    const sbtProc = sbtPromise.childProcess
    sbtProc.on('close', (code: number) => {
      if (code != 0) {
        const msg = "Configuring the IDE failed."
        outputChannel.append(msg)
        throw new Error(msg)
      }
    })

    return sbtPromise
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
