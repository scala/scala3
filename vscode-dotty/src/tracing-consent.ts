import { Memento } from 'vscode';

export type TracingConsent = 'yes' | 'no' | 'no-answer';

export class TracingConsentCache {
    private readonly workspaceState: Memento

    // since updating Memento is async, caching prevents nonsense edge-cases
    private cache?: TracingConsent
    private subscribers: Array<() => void> = [];

    constructor(workspaceState: Memento) {
        this.workspaceState = workspaceState;
    }

    get(): TracingConsent {
      if (this.cache !== undefined) return this.cache;
      const setting = this.workspaceState.get('remote-tracing-consent');
      this.cache = setting === undefined ? 'no-answer'
        : setting ? 'yes'
        : 'no';
      return this.cache;
    }

    set(value: 'yes' | 'no'): void {
      this.workspaceState.update('remote-tracing-consent', value === 'yes');
      this.cache = value;
      this.subscribers.forEach(f => f());
    }

    subscribe(callback: () => void): void {
      this.subscribers.push(callback);
    }
}
