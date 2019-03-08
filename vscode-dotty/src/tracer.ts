import * as vscode from 'vscode'

import * as fs from 'fs'
import * as path from 'path'

import * as archiver from 'archiver'
import * as WebSocket from 'ws'
import * as request from 'request'

import { extensionName } from './extension'
import { TracingConsentCache } from './tracing-consent'

const consentCommandName = `${extensionName}.adjust-consent`

export interface Ctx {
  readonly extensionContext: vscode.ExtensionContext,
  readonly extensionConfig: vscode.WorkspaceConfiguration,
  readonly extensionOut: vscode.OutputChannel
}

export class Tracer {
    private readonly ctx: Ctx

    private projectId: string
    private machineId: string
    private sessionId: string

    private tracingConsent: TracingConsentCache

    private readonly remoteTracingUrl: string | undefined
    private readonly remoteWorkspaceDumpUrl: string | undefined
    private readonly maximumMessageSize: number
    private get isTracingEnabled(): boolean {
        return Boolean(this.remoteWorkspaceDumpUrl || this.remoteTracingUrl)
    }

    constructor(ctx: Ctx) {
        this.ctx = ctx

        this.tracingConsent = new TracingConsentCache(ctx.extensionContext.workspaceState)

        this.remoteWorkspaceDumpUrl = this.ctx.extensionConfig.get<string>('trace.remoteWorkspaceDumpUrl')
        this.remoteTracingUrl = this.ctx.extensionConfig.get<string>('trace.remoteTracingUrl')
        const maximumMessageSize = this.ctx.extensionConfig.get<number>('trace.maximumMessageSize')
        this.maximumMessageSize = maximumMessageSize === undefined || maximumMessageSize < 0 ? 0 : maximumMessageSize | 0

        this.machineId = (() => {
            const machineIdKey = 'trace.machineId'
            function persisted(value: string): string {
                ctx.extensionConfig.update(machineIdKey, value, vscode.ConfigurationTarget.Global)
                return value
            }

            const machineId = ctx.extensionConfig.get<string | null>(machineIdKey)
            if (machineId != null) return machineId

            // vscode.env.machineId is a dummy value if telemetry is off - cannot be used
            const vscodeMachineId = vscode.workspace.getConfiguration().get<string>('telemetry.machineId')
            if (vscodeMachineId !== undefined) return persisted(vscodeMachineId)

            function uuidv4() {
                // https://stackoverflow.com/a/2117523
                return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function (c) {
                    var r = Math.random() * 16 | 0, v = c == 'x' ? r : (r & 0x3 | 0x8)
                    return v.toString(16)
                })
            }

            return persisted(uuidv4())
        })()

        if (vscode.workspace.name !== undefined) {
          // HACK: The projects cloned by students use the naming convention
          // `$id-$name-$githubUsername`, so they leak the student's github
          // username, to preserve anonymity, we drop the last part of the
          // name.
          this.projectId = vscode.workspace.name.replace(/^(\d+-.+)-.+$/, "$1")
        } else {
          this.projectId = 'no-project'
        }

        this.sessionId = new Date().toISOString()
    }

    run(): vscode.OutputChannel | undefined {
        const consentCommandDisposable = vscode.commands.registerCommand(consentCommandName, () => this.askForTracingConsent())
        if (this.isTracingEnabled && this.tracingConsent.get() === 'no-answer') this.askForTracingConsent()
        this.initializeAsyncWorkspaceDump()

        const lspOutputChannel = this.createLspOutputChannel()
        const statusBarItem = this.createStatusBarItem()
        for (const disposable of [consentCommandDisposable, lspOutputChannel, statusBarItem]) {
            if (disposable) this.ctx.extensionContext.subscriptions.push(disposable)
        }
        return lspOutputChannel
    }

    private askForTracingConsent(): void {
        vscode.window.showInformationMessage(
            'To help us improve the Scala IDE support, we would like to collect ' +
            'the content of every Scala file in your project and ' +
            'every interaction with Scala files in the IDE, including keystrokes. ' +
            'This data will be stored anonymously (we won\'t know your name) on servers at EPFL in Switzerland.',
            { 'modal': true },
            { title: 'Allow' },
            { title: 'Deny', isCloseAffordance: true }
        ).then(value => {
            if (value !== undefined && (value.title === 'Allow' || value.title === 'Deny')) this.tracingConsent.set(value.title)
        })
    }

    private initializeAsyncWorkspaceDump() {
        const url = this.remoteWorkspaceDumpUrl
        if (!url) return

        const doInitialize = () => {
            try {
                this.asyncUploadWorkspaceDump(url)
            } catch (err) {
                this.logError('error during workspace dump', safeError(err))
            }
        }

        if (this.tracingConsent.get() === 'Allow') {
            doInitialize()
        } else {
            let didInitialize = false
            this.tracingConsent.subscribe(() => {
                if (didInitialize) return
                didInitialize = true
                doInitialize()
            })
        }
    }

    private createStatusBarItem(): vscode.StatusBarItem | undefined {
        if (!this.isTracingEnabled) return undefined
        const item = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 0)
        item.command = consentCommandName
        const renderStatusBarItem = () => {
            item.text = (() => {
                const desc = this.tracingConsent.get() === 'Allow' ? 'On' : 'Off'
                return `$(radio-tower) Scala telemetry: ${desc}`
            })()

            item.tooltip = (() => {
                const desc = this.tracingConsent.get() === 'Allow' ? 'enabled' : 'disabled'
                const toggle = this.tracingConsent.get() === 'Allow' ? 'disable' : 'enable'
                return `Data collection for Scala is ${desc}. ` +
                    `Click to ${toggle} it.`
            })()
        }
        renderStatusBarItem()
        this.tracingConsent.subscribe(renderStatusBarItem)
        item.show()
        return item
    }

    private createLspOutputChannel(): vscode.OutputChannel | undefined {
        if (this.ctx.extensionConfig.get('trace.server') === undefined) {
            if (this.remoteTracingUrl) this.ctx.extensionOut.appendLine(
                'error: tracing URL is set, but trace.server not - no remote tracing possible.'
            )
            return undefined
        }

        const lspOutputChannel = vscode.window.createOutputChannel('Dotty LSP Communication')
        if (!this.remoteTracingUrl) return lspOutputChannel
        try {
            return this.createRemoteLspOutputChannel(this.remoteTracingUrl, lspOutputChannel)
        } catch (err) {
            this.logError('error during remote output channel creation', safeError(err))
            return lspOutputChannel
        }
    }

    private asyncUploadWorkspaceDump(url: string) {
        const storagePath = this.ctx.extensionContext.storagePath
        // TODO: handle multi-root workspaces
        const rootPath = vscode.workspace.rootPath
        if (storagePath === undefined || rootPath === undefined) {
            this.logError('Cannot start workspace dump b/c of workspace state:', { storagePath, rootPath })
            return
        }

        if (!fs.existsSync(storagePath)) fs.mkdirSync(storagePath)
        const outputPath = path.join(storagePath, 'workspace-dump.zip')
        if (fs.existsSync(outputPath)) fs.unlinkSync(outputPath)
        const output = fs.createWriteStream(outputPath)

        const zip = archiver('zip')
        zip.on('error', (err) => this.logError('zip error', safeError(err)))
        zip.on('warning', (err) => this.logError('zip warning', safeError(err)))
        zip.on('finish', () => {
            this.ctx.extensionOut.appendLine('zip - finished')
            fs.createReadStream(outputPath).pipe(
                request.put(url, {
                    qs: {
                        client: this.machineId,
                        project: this.projectId,
                        session: this.sessionId
                    }
                })
                    .on('error', (err) => this.logError('zip upload connection error', url, safeError(err)))
                    .on('complete', (resp) => {
                        if (!(resp.statusCode >= 200 && resp.statusCode < 300)) {
                            this.logError('zip upload http error', url, resp.statusCode, resp.body)
                        } else {
                            this.ctx.extensionOut.appendLine('zip - http upload finished')
                        }
                    })
            )
        })

        this.ctx.extensionOut.appendLine('zip - starting')
        zip.pipe(output)
        zip.glob('./**/*.{scala,sc,sbt,java}', { cwd: rootPath })
        zip.glob('./**/.dotty-ide{.json,-artifact}', { cwd: rootPath })
        zip.finalize()
    }

    private createRemoteLspOutputChannel(
        remoteTracingUrl: string,
        localOutputChannel: vscode.OutputChannel
    ): vscode.OutputChannel {
        const createSocket = () => {
            const socket = new WebSocket(remoteTracingUrl, {
                headers: {
                    'X-DLS-Project-ID': this.projectId,
                    'X-DLS-Client-ID': this.machineId,
                    'X-DLS-Session-ID': this.sessionId,
                },
            })

            const timer = setInterval(
                () => {
                    if (socket.readyState === WebSocket.OPEN) {
                        socket.send('')
                    } else if (socket.readyState === WebSocket.CLOSED) {
                        clearInterval(timer)
                    }
                },
                10 * 1000 /*ms*/,
            )

            socket.onerror = (event) => {
                this.logError(
                    'socket error',
                    remoteTracingUrl,
                    new SafeJsonifier(event, (event) => ({
                        error: safeError(event.error),
                        message: event.message,
                        type: event.type
                    }))
                )
            }

            socket.onclose = (event) => {
                this.logError(
                    'socket closed',
                    remoteTracingUrl,
                    new SafeJsonifier(event, (event) => ({
                        wasClean: event.wasClean,
                        code: event.code,
                        reason: event.reason
                    }))
                )
            }

            return socket
        }

        let alreadyCreated = false
        let socket: WebSocket
        // note: creating socket lazily is important for correctness
        // if the user did not initially give his consent on IDE start, but gives it afterwards
        // we only want to start a connection and upload data *after* being given consent
        const withSocket: (thunk: (socket: WebSocket) => any) => void = (thunk) => {
            // only try to create the socket _once_ to avoid endlessly looping
            if (!alreadyCreated) {
                alreadyCreated = true
                try {
                    socket = createSocket()
                } catch (err) {
                    this.logError('socket create error', safeError(err))
                }
            }

            if (socket) thunk(socket)
        }

        let log: string = ''
        let messageCounter: number = 0
        // Avoid filling the user memory with log messages
        let maxLocalMessages: number = 1000
        return {
          name: 'websocket',

          append: (value: string) => {
	    messageCounter++
	    if (messageCounter > maxLocalMessages) {
	      messageCounter = 0
	      localOutputChannel.clear()
	    }

            localOutputChannel.append(value)
            if (this.tracingConsent.get() === 'Deny') return
            log += value
          },

          appendLine: (value: string) => {
	    messageCounter++
	    if (messageCounter > maxLocalMessages) {
	      messageCounter = 0
	      localOutputChannel.clear()
	    }

            localOutputChannel.appendLine(value)
            if (this.tracingConsent.get() === 'Deny') {
              log = ''
              return
            }

            log += value
            log += '\n'
            if (this.tracingConsent.get() === 'Allow') withSocket((socket) => {
              if (socket.readyState === WebSocket.OPEN) {
                const send = (msg: string) => socket.send(msg, (err) => {
                  if (err) {
                    this.logError('socket send error', err)
                  }
                })

                let start = 0
                while (start < log.length) {
                  send(log.substring(start, start + this.maximumMessageSize))
                  start += this.maximumMessageSize
                }
                log = ''
              }
            })
          },

          clear() { },
          show() { },
          hide() { },
          dispose() {
            if (socket) socket.close()
            localOutputChannel.dispose()
          }
        }
    }

    private logError(message: string, ...rest: any[]) {
        const msg = `[Dotty LSP Tracer] ${message}`
        // in a browser, we'd be able to log a SafeJsonifier directly
        // and get an inspectable object in the console
        // unfortunately, Electron apparently uses .toJson if available,
        // so we need to manually unwrap SafeJsonifiers
        console.error(msg, ...rest.map((a) => a instanceof SafeJsonifier ? a.value : a))
        function cautiousStringify(a: any): string  {
            try {
                return JSON.stringify(a, undefined, 4)
            } catch (err) {
                console.error('cannot stringify', err, a)
                return a.toString()
            }
        }
        this.ctx.extensionOut.appendLine([msg].concat(rest.map(cautiousStringify)).join(' '))
    }
}

function safeError(e: Error): SafeJsonifier<Error> {
    return new SafeJsonifier(e, (e) => e.toString())
}

/**
 * Wraps a value of type T so it's possible to safely pass it to JSON.stringify.
 * 
 * Values with circular references (errors, for example) cause JSON.stringify to throw an exception
 */
class SafeJsonifier<T> {
    constructor(
        readonly value: T,
        readonly valueToObject: (t: T) => {}
    ) {}

    toJSON() {
        return this.valueToObject(this.value)
    }
}
