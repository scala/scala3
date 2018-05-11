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

  const sbtArtifact = "org.scala-sbt:sbt-launch:1.1.4"
  const languageServerArtifactFile = `${vscode.workspace.rootPath}/.dotty-ide-artifact`
  const languageServerDefaultArtifact = "ch.epfl.lamp:dotty-language-server_0.8:0.8.0-RC1"
  const loadPluginArtifact = "ch.epfl.scala:load-plugin_2.12:0.1.0+2-496ac670"
  fs.readFile(languageServerArtifactFile, (err, data) => {
    const languageServerArtifact = err ? languageServerDefaultArtifact : data.toString().trim()

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
      fetchAndRun(sbtArtifact, languageServerArtifact, loadPluginArtifact)
    }
  })
}

function fetchAndRun(sbtArtifact: string, languageServerArtifact: string, loadPluginArtifact: string) {
  const coursierPath = path.join(extensionContext.extensionPath, './out/coursier');

  const sbtPromise = fetchWithCoursier(coursierPath, sbtArtifact)
  const languageServerPromise = fetchWithCoursier(coursierPath, languageServerArtifact)
  const loadPluginPromise = fetchWithCoursier(coursierPath, loadPluginArtifact)

  Promise.all([sbtPromise, languageServerPromise, loadPluginPromise]).then((results) => {
    const [sbtClasspath, languageServerClasspath, loadPluginJar] = results
    return configureIDE(sbtClasspath, languageServerClasspath, loadPluginJar)
  }).then((languageServerClasspath) => {
    run({
      command: "java",
      args: ["-classpath", languageServerClasspath, "dotty.tools.languageserver.Main", "-stdio"]
    })
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
        let msg = data.toString()
        outputChannel.append(msg)
      })

      coursierProc.on('close', (code: number) => {
        if (code != 0) {
          let msg = `Couldn't fetch '${ artifact }' (exit code ${ code }).`
          outputChannel.append(msg)
          throw new Error(msg)
        }
      })
      return coursierPromise.then(() => {
        return classPath;
      });
    })
}

function configureIDE(sbtClasspath: string, languageServerClasspath: string, loadPluginJar: string) {
  return vscode.window.withProgress({
    location: vscode.ProgressLocation.Window,
    title: 'Configuring IDE...'
  }, (progress) => {
    const applyLoadPlugin = `apply -cp ${ loadPluginJar } ch.epfl.scala.loadplugin.LoadPlugin`
    const ifAbsentCommands = [
      "if-absent dotty.tools.sbtplugin.DottyPlugin",
      "\"set every scalaVersion := \\\"0.8.0-RC1\\\"\"",
      "\"load-plugin ch.epfl.lamp:sbt-dotty:0.2.2 dotty.tools.sbtplugin.DottyPlugin\"",
      "\"load-plugin ch.epfl.lamp:sbt-dotty:0.2.2 dotty.tools.sbtplugin.DottyIDEPlugin\""
    ].join(" ")
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

    return sbtPromise.then(() => { return languageServerClasspath });
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
