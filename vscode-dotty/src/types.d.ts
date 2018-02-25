declare module 'child-process-promise' {

  import {ChildProcess} from "child_process";

  interface ChildPromiseResult {
    code: number;
  }

  interface ChildProcessPromise extends Promise<ChildPromiseResult> {
    childProcess: ChildProcess;
  }

  function spawn(command: string, args: string[]): ChildProcessPromise;
}
