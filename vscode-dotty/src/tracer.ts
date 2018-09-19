import * as vscode from 'vscode';

import * as fs from 'fs';
import * as path from 'path';

import * as archiver from 'archiver';
import * as WebSocket from 'ws';
import * as request from 'request';

import { TracingConsentCache } from './tracing-consent';

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

    private remoteTracingUrl?: string

    constructor(ctx: Ctx) {
        this.ctx = ctx;

        this.tracingConsent = new TracingConsentCache(ctx.extensionContext.workspaceState);

        this.machineId = (() => {
            const machineIdKey = 'tracing.machineId';
            function persisted(value: string): string {
                ctx.extensionConfig.update(machineIdKey, value, vscode.ConfigurationTarget.Global)
                return value
            }

            const machineId = ctx.extensionConfig.get<string | null>(machineIdKey)
            if (machineId != null) return machineId;

            // vscode.env.machineId is a dummy value if telemetry is off - cannot be used
            const vscodeMachineId = vscode.workspace.getConfiguration().get<string>('telemetry.machineId')
            if (vscodeMachineId !== undefined) return persisted(vscodeMachineId)

            function uuidv4() {
                // https://stackoverflow.com/a/2117523
                return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function (c) {
                    var r = Math.random() * 16 | 0, v = c == 'x' ? r : (r & 0x3 | 0x8);
                    return v.toString(16);
                });
            }

            return persisted(uuidv4())
        })();

        this.projectId = vscode.workspace.name !== undefined ? vscode.workspace.name : 'no-project';
        this.sessionId = new Date().toISOString();
    }

    initializeAsyncWorkspaceDump() {
        const remoteWorkspaceDumpUrl = this.ctx.extensionConfig.get<string>('remoteWorkspaceDumpUrl');
        if (remoteWorkspaceDumpUrl === undefined) return;

        try {
            this.asyncUploadWorkspaceDump(remoteWorkspaceDumpUrl);
        } catch (err) {
            this.logError('error during workspace dump', safeError(err));
        }
    }

    createLspOutputChannel(): vscode.OutputChannel | undefined {
        const remoteTracingUrl = this.ctx.extensionConfig.get<string>('remoteTracingUrl');
        if (!remoteTracingUrl) return undefined;

        if (this.tracingConsent.get() === 'no-answer') {
            vscode.window.showInformationMessage(
                'Do you want to help EPFL develop this plugin by uploading your usage data? ' +
                'PLEASE BE AWARE that this will upload all of your keystrokes and all of your code, ' +
                'among other things.',
                'yes', 'no'
            ).then((value: string | undefined) => {
                if (value === 'yes' || value === 'no') this.tracingConsent.set(value);
            });
        }

        const localLspOutputChannel = vscode.window.createOutputChannel('Dotty LSP Communication')
        try {
            return this.createRemoteLspOutputChannel(remoteTracingUrl, localLspOutputChannel);
        } catch (err) {
            this.logError('error during remote output channel creation', safeError(err));
            return localLspOutputChannel;
        }
    }

    private asyncUploadWorkspaceDump(url: string) {
        const storagePath = this.ctx.extensionContext.storagePath;
        const rootPath = vscode.workspace.rootPath;
        if (storagePath === undefined || rootPath === undefined) {
            this.logError('Cannot start workspace dump b/c of workspace state:', { storagePath, rootPath });
            return;
        }

        if (!fs.existsSync(storagePath)) fs.mkdirSync(storagePath);
        const outputPath = path.join(storagePath, 'workspace-dump.zip');
        if (fs.existsSync(outputPath)) fs.unlinkSync(outputPath);
        let output = fs.createWriteStream(outputPath);
        output.on('end', () => {
            this.ctx.extensionOut.appendLine('zip - data has been drained');
        });

        const zip = archiver('zip');
        zip.on('error', (err) => this.logError('zip error', safeError(err)));
        zip.on('warning', (err) => this.logError('zip warning', safeError(err)));
        zip.on('entry', (entry) => {
            this.ctx.extensionOut.appendLine(`zip - entry: ${entry.name}`);
        });
        zip.on('finish', () => {
            this.ctx.extensionOut.appendLine('zip - finished');
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
                            this.logError('zip upload http error', url, resp.statusCode, resp.body);
                        } else {
                            this.ctx.extensionOut.appendLine('zip - http upload finished');
                        }
                    })
            );
        });
        zip.pipe(output);
        zip.glob('./**/*.{scala,sbt}', { cwd: rootPath });
        zip.finalize();
    }

    private createRemoteLspOutputChannel(
        remoteTracingUrl: string,
        localOutputChannel: vscode.OutputChannel
    ): vscode.OutputChannel {
        const socketHeaders = {
            'X-DLS-Project-ID': this.projectId,
            'X-DLS-Client-ID': this.machineId,
            'X-DLS-Session-ID': this.sessionId,
        };

        const socket = new WebSocket(remoteTracingUrl, { headers: socketHeaders });

        const timer = setInterval(
            () => {
                if (socket.readyState === WebSocket.OPEN) {
                    socket.send('');
                } else if (socket.readyState === WebSocket.CLOSED) {
                    clearInterval(timer);
                }
            },
            10 * 1000 /*ms*/,
        )

        socket.onerror = (event) => {
            this.logErrorWithoutNotifying(
                'socket error',
                remoteTracingUrl,
                new SafeJsonifier(event, (event) => ({
                    error: safeError(event.error),
                    message: event.message,
                    type: event.type
                }))
            );
            vscode.window.showWarningMessage('An error occured in Dotty LSP remote tracing connection.');
        }

        socket.onclose = (event) => {
            this.logErrorWithoutNotifying(
                'socket closed',
                remoteTracingUrl,
                new SafeJsonifier(event, (event) => ({
                    wasClean: event.wasClean,
                    code: event.code,
                    reason: event.reason
                }))
            );
            vscode.window.showWarningMessage('Dotty LSP remote tracing connection was dropped.');
        }

        let log: string = '';
        return {
            name: 'websocket',

            append: (value: string) => {
                localOutputChannel.append(value);
                if (this.tracingConsent.get() === 'no') return;
                log += value;
            },

            appendLine: (value: string) => {
                localOutputChannel.appendLine(value)
                if (this.tracingConsent.get() === 'no') {
                    log = '';
                    return;
                }

                log += value;
                log += '\n';
                if (this.tracingConsent.get() === 'yes' && socket.readyState === WebSocket.OPEN) {
                    socket.send(log, (err) => {
                        if (err) {
                            this.logError('socket send error', err)
                        }
                    });
                    log = '';
                }
            },

            clear() { },
            show() { },
            hide() { },
            dispose() {
                socket.close();
                localOutputChannel.dispose();
            }
        };
    }

    private silenceErrors: boolean = false;
    private logErrorWithoutNotifying(message: string, ...rest: any[]) {
        const msg = `[Dotty LSP Tracer] ${message}`;
        // unwrap SafeJsonifier, for some reason Electron logs the result
        // of .toJSON, unlike browsers
        console.error(msg, ...rest.map((a) => a instanceof SafeJsonifier ? a.value : a));
        function cautiousStringify(a: any): string  {
            try {
                return JSON.stringify(a, undefined, 4);
            } catch (err) {
                console.error('cannot stringify', err, a);
                return a.toString();
            }
        }
        this.ctx.extensionOut.appendLine([msg].concat(rest.map(cautiousStringify)).join(' '));
    }
    private logError(message: string, ...rest: any[]) {
        this.logErrorWithoutNotifying(message, ...rest);
        if (!this.silenceErrors) {
            vscode.window.showErrorMessage(
                'An error occured which prevents sending usage data to EPFL. ' +
                'Please copy the text from "Dotty Language Client" output (View > Output) and send it to your TA.',
                'Silence further errors'
            ).then((result) => {
                if (result !== undefined) {
                    this.silenceErrors = true;
                }
            })
        }
    }
}

function safeError(e: Error): SafeJsonifier<Error> {
    return new SafeJsonifier(e, (e) => e.toString());
}

class SafeJsonifier<T> {
    value: T
    valueToObject: (t: T) => {}

    constructor(value: T, valueToObject: (t: T) => {}) {
        this.value = value;
        this.valueToObject = valueToObject;
    }

    toJSON() {
        return this.valueToObject(this.value);
    }
}
