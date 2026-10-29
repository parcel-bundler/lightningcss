import {spawn} from 'node:child_process';
import readline from 'node:readline';

// One request at a time; restart after an abort/timeout, retaining the case ID
// in the orchestrator. Debug builds also catch recoverable Rust panics.
export class Adapter {
  constructor(executable, timeout = 5000, args = []) {
    this.executable = executable;
    this.timeout = timeout;
    this.args = args;
    this.sequence = 0;
  }
  start() {
    const child = (this.child = spawn(this.executable, this.args, {
      stdio: ['pipe', 'pipe', 'pipe'],
    }));
    this.stderr = '';
    child.stderr.on('data', (data) => {
      if (child !== this.child) return;
      this.stderr = (this.stderr + data).slice(-8000);
    });
    child.stdin.on('error', () => {});
    const lines = readline.createInterface({input: child.stdout});
    lines.on('line', (line) => {
      if (child !== this.child) return;
      if (!this.pending) { this.close(); return; }
      try {
        const response = JSON.parse(line);
        const successes = ['typed', 'unparsed', 'css-wide', 'custom-property', 'unknown-property', 'deferred'];
        if (!response || typeof response !== 'object' ||
            response.requestId !== this.requestId ||
            Object.keys(response).some(key => !['requestId', 'recognition', 'code', 'error'].includes(key)) ||
            !(successes.includes(response.recognition)
              ? typeof response.code === 'string' && !Object.hasOwn(response, 'error')
              : ['error', 'panic'].includes(response.recognition) && typeof response.error === 'string' &&
                !Object.hasOwn(response, 'code')))
          throw Error('Invalid or mismatched adapter response');
        const {requestId, ...result} = response;
        this.finish(result);
      } catch (error) {
        this.finish({recognition: 'protocol-error', error: `${error.message}: ${line}`});
        this.close();
      }
    });
    child.on('error', (error) => {
      if (child === this.child) {
        this.finish({recognition: 'protocol-error', error: error.message});
        this.child = null;
      }
    });
    child.on('exit', (code, signal) => {
      if (child !== this.child) return;
      this.finish({
        recognition: 'crash',
        error: `exit ${code}, signal ${signal}`,
        stderr: this.stderr,
      });
      this.child = null;
    });
  }
  finish(value) {
    if (!this.pending) return;
    clearTimeout(this.timer);
    const resolve = this.pending;
    this.pending = null;
    resolve(value);
  }
  request(input) {
    if (this.pending)
      throw Error('Concurrent adapter requests are unsupported');
    const requestId = ++this.sequence;
    // Serialize before opening a process or installing pending-request state.
    const line = JSON.stringify({...input, requestId}) + '\n';
    if (!this.child) this.start();
    return new Promise((resolve) => {
      this.pending = resolve;
      this.requestId = requestId;
      this.timer = setTimeout(() => {
        this.finish({recognition: 'timeout', stderr: this.stderr});
        this.close();
      }, this.timeout);
      this.child.stdin.write(line);
    });
  }
  close() {
    this.finish({recognition: 'protocol-error', error: 'Adapter closed before responding'});
    this.child?.kill('SIGKILL');
    this.child = null;
  }
}
