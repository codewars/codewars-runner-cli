"use strict";

const exec = require('child_process').exec;
const fs = require('fs');

module.exports = {
  solutionOnly(opts, runCode, fail) {
    const args = [
      "swiftc",
      "-Xlinker", "-rpath",
      "-Xlinker", "/usr/lib/swift/linux",
      "-L", "/usr/lib/swift/linux",
      "-I", "/usr/lib/swift/linux",
      "-Xcc", "-fblocks",
      "-o", "/workspace/a.out",
    ];

    if (opts.setup) {
      fs.writeFileSync('/workspace/setup.swift', opts.setup);
      args.push('/workspace/setup.swift');
    }
    fs.writeFileSync('/workspace/main.swift', opts.solution);
    args.push('/workspace/main.swift');

    return exec(args.join(' '), (err, stdout, stderr) => {
      if (err) return fail(Object.assign(err, {stdout, stderr}));
      runCode({
        name: '/workspace/a.out',
        args: []
      });
    });
  },

  testIntegration(opts, runCode, fail) {
    if (opts.testFramework != 'xctest')
      throw new Error(`Test framework '${opts.testFramework}' is not supported`);

    const args = [
      "swiftc",
      "-Xlinker", "-rpath",
      "-Xlinker", "/usr/lib/swift/linux",
      "-L", "/usr/lib/swift/linux",
      "-I", "/usr/lib/swift/linux",
      "-Xcc", "-fblocks",
      "-o", "/workspace/a.out",
      "/runner/frameworks/swift/xctest/_XCTMain.swift",
      "/runner/frameworks/swift/xctest/CodewarsObserver.swift",
    ];

    if (opts.setup) {
      fs.writeFileSync('/workspace/setup.swift', opts.setup);
      args.push('/workspace/setup.swift');
    }
    fs.writeFileSync('/workspace/solution.swift', opts.solution);
    fs.writeFileSync('/workspace/main.swift', opts.fixture.replace(/^\s*XCTMain\s*\(/m, '_XCTMain('));
    args.push('/workspace/solution.swift', '/workspace/main.swift');

    return exec(args.join(' '), (err, stdout, stderr) => {
      if (err) return fail(Object.assign(err, {stdout, stderr}));
      runCode({
        name: '/workspace/a.out',
        args: []
      });
    });
  },
};
