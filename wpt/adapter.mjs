import {spawn} from 'node:child_process';
import readline from 'node:readline';

// One request at a time; restart after an abort/timeout, retaining the case ID
// in the orchestrator. Debug builds also catch recoverable Rust panics.
export class Adapter {
  constructor(executable, timeout = 5000, args = []) {
    this.executable = executable;
    this.timeout = timeout;
    this.args = args;
  }
  start() {
    const child = (this.child = spawn(this.executable, this.args, {
      stdio: ['pipe', 'pipe', 'pipe'],
    }));
    this.stderr = '';
    child.stderr.on('data', (data) => {
      this.stderr = (this.stderr + data).slice(-8000);
    });
    child.stdin.on('error', () => {});
    const lines = readline.createInterface({input: child.stdout});
    lines.on('line', (line) => {
      if (child !== this.child || !this.pending) return;
      try {
        this.finish(JSON.parse(line));
      } catch {
        this.finish({recognition: 'protocol-error', error: line});
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
    if (!this.child) this.start();
    return new Promise((resolve) => {
      this.pending = resolve;
      this.timer = setTimeout(() => {
        this.finish({recognition: 'timeout', stderr: this.stderr});
        this.close();
      }, this.timeout);
      this.child.stdin.write(JSON.stringify(input) + '\n');
    });
  }
  close() {
    this.child?.kill('SIGKILL');
    this.child = null;
  }
}
