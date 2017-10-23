"use strict";

const fs = require('fs');

module.exports = {
  solutionOnly(opts, runCode, fail) {
    fs.writeFileSync('/workspace/rust/src/main.rs',
      `${opts.setup ? opts.setup+'\n' : ''}${opts.solution}`);
    runCode({
      name: 'cargo',
      args: ['run'],
      options: {
        cwd: '/workspace/rust',
      },
    });
  },

  testIntegration(opts, runCode, fail) {
    if (opts.testFramework != 'rust')
      throw new Error(`Test framework '${opts.testFramework}' is not supported`);

    //
    // needs some hack in order to maintain backward compatibility and avoid errors with duplicate imports
    // .
    // |- Cargo.toml
    // `- src
    //    |- lib.rs    (opt.setup + opts.solution + module declaration)
    //    `- tests.rs  (opts.fixture)
    // - placing `opts.fixture` in submodule `tests.rs` should avoid issues with duplicate imports
    // - maintains backward compatibility by prepending `use super::*;` to `opts.fixture` if missing
    //
    fs.writeFileSync('/workspace/rust/src/lib.rs',
      `${opts.setup ? opts.setup+'\n' : ''}${opts.solution}\n#[cfg(test)]\nmod tests;`);
    var fixture = opts.fixture;
    if (!fixture.includes('use super::*;')) { // HACK backward compatibility
      fixture = 'use super::*;\n' + fixture;
    }
    fs.writeFileSync('/workspace/rust/src/tests.rs', fixture);
    runCode({
      name: 'cargo',
      args: ['test'],
      options: {
        cwd: '/workspace/rust',
      },
    });
  },

  transformBuffer(opts, buffer) {
    if (!opts.fixture) return;

    const ss = buffer.stdout.split(/\n((?:---- \S+ stdout ----)|failures:)\n/);
    // Save test failures in object to use in output.
    // There shouldn't be name collisions since test cases are rust functions.
    const failures = {};
    for (let i = 1; i < ss.length; ++i) {
      const s = ss[i];
      const m = s.match(/^---- (\S+) stdout ----$/);
      if (m === null) continue;
      const name = m[1];
      const fail = [];
      const x = ss[++i];
      const m2 = x.match(/thread '[^']+' panicked at '(.+)'.*/);
      if (m2.index != 1) fail.push(x.slice(1, m2.index)); // user logged output
      fail.push(`<FAILED::>Test Failed<:LF:>${m2[1].replace(/\\'/g, "'")}`);
      failures[name] = fail;
    }

    const out = [];
    for (const s of ss[0].split('\n')) {
      const m = s.match(/^test (\S+) \.{3} (FAILED|ok)$/);
      if (m === null) continue;
      out.push(`<IT::>${m[1].replace(/^tests::/, '')}`);
      if (m[2] == 'ok') {
        out.push('<PASSED::>Test Passed');
      }
      else {
        out.push.apply(out, failures[m[1]]);
      }
      out.push(`<COMPLETEDIN::>`);
    }
    out.push('');
    buffer.stdout = out.join('\n');
  },

  sanitizeStdErr(opts, stderr) {
    // remove logs from cargo test
    stderr = stderr
      .replace(/^\s+Compiling .*$/gm, '')
      .replace(/^\s+Finished .*$/m, '')
      .replace(/^\s+Running .*$/m, '')
      .replace(/^\s+Doc-tests .*$/m, '')
      .replace('To learn more, run the command again with --verbose.', '')
      .replace(/^error: test failed$/m, '')
      .replace(/^error: Could not compile .*$/m, '')
      .trim();
    if (stderr !== '') stderr += '\n';
    if (/^error: /m.test(stderr) && opts.setup) {
      stderr += "\nNOTE: Line numbers reported within errors will not match up exactly to those shown within your editors due to concatenation.\n";
    }
    return stderr;
  },
};
