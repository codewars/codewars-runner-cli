"use strict";

const fs = require('fs-extra');

module.exports = {
  modifyOpts(opts) {
    opts.services = ['testrpc'];
  },

  // solutionOnly(opts, runCode) {
  // },

  testIntegration(opts, runCode, fail) {
    fs.outputFileSync('/workspace/solidity/contracts/solution.sol', opts.solution);
    fs.outputFileSync('/workspace/solidity/test/fixture.js', opts.fixture);

    // collect declared libraries and contracts to generate `2_deploy_contracts.js`.
    // TODO link library to library?
    const libs = [], contracts = [];
    if (opts.setup) {
      fs.outputFileSync('/workspace/solidity/contracts/setup.sol', opts.setup);
      collectLibraries(opts.setup, libs);
      collectContracts(opts.setup, contracts);
    }
    collectLibraries(opts.solution, libs);
    collectContracts(opts.solution, contracts);

    const deploy = [];
    for (const x of libs) deploy.push(`var ${x} = artifacts.require('${x}');`);
    for (const x of contracts) deploy.push(`var ${x} = artifacts.require('${x}');`);
    deploy.push('module.exports = function(deployer) {');
    for (const x of libs) {
      deploy.push(`  deployer.deploy(${x});`);
      // for each library, link to all contracts.
      // if a contract doesn't rely on the library, it will be ignored
      if (contracts.length > 0) deploy.push(`  deployer.link(${x}, [${contracts.join(',')}]);`);
    }
    // switch to using `deployer.autolink();` available on Truffle Beta when it's stable.
    // deploy.push('  deployer.autolink();');
    for (const x of contracts) deploy.push(`  deployer.deploy(${x});`);
    deploy.push('};');
    fs.outputFileSync('/workspace/solidity/migrations/2_deploy_contracts.js', deploy.join('\n'));

    runCode({
      name: 'truffle',
      args: ['test'],
      options: {
        cwd: '/workspace/solidity',
      },
    });
  },

  sanitizeStdOut(opts, stdout) {
    return stdout.replace(/^Compiling \.\/.*\n/gm, '');
  },
};

function collectContracts(code, out) {
  const m = code.match(/^\s*contract\s+\S+/gm);
  if (m !== null) out.push.apply(out, m.map(c => c.replace(/^\s*contract\s+/, '')));
}
function collectLibraries(code, out) {
  const m = code.match(/^\s*library\s+\S+/gm);
  if (m !== null) out.push.apply(out, m.map(c => c.replace(/^\s*library\s+/, '')));
}

/*
// used to parse and format outputs of `--verbose-rpc`, formatRPC(stdout) in sanitizeStdOut
function formatRPC(out) {
  const reqs = new Map();
  return out.replace(/\n? {3}> \{[\s\S]*?\n {3}> \}(?=\n)/g, m => req(m.replace(/^ {3}> /gm, '')))
    .replace(/\n? {3}> \[[\s\S]*?\n {3}> \](?=\n)/g, m => req(m.replace(/^ {3}> /gm, '')))
    .replace(/\n? < {3}\{[\s\S]*?\n < {3}\}(?=\n)/g, m => res(m.replace(/^ < {3}/gm, '')))
    .replace(/\n? < {3}\[[\s\S]*?\n < {3}\](?=\n)/g, m => res(m.replace(/^ < {3}/gm, '')));

  function req(s) {
    try {
      const o = JSON.parse(s);
      const rs = Array.isArray(o) ? o : [o];
      for (const r of rs) reqs.set(r.id, r);
      return '';
    }
    catch (_) {
      return s;
    }
  }

  function res(s) {
    const out = [];
    try {
      const o = JSON.parse(s);
      const rs = Array.isArray(o) ? o : [o];
      for (const r of rs) {
        if (reqs.has(r.id)) {
          const req = reqs.get(r.id);
          const method = req.method;
          const params = req.params;
          const result = r.result;
          if (method === 'net_version') {
            out.push(`\n<LOG::net_version (${r.id})>${result}`);
          } else if (method === 'eth_accounts') {
            out.push(`\n<LOG:TABLE:eth_accounts (${r.id})>${JSON.stringify(r.result.map((r, i) => ({'#': i, account: r})))}`);
          } else {
            out.push(`\n<LOG:TABLE:${method} Request>[${JSON.stringify({method, params})}]`);
            if (typeof result == 'object') {
              out.push(`\n<TAB:TABLE:Response>[${JSON.stringify(result)}]`);
            } else {
              out.push(`\n<TAB:TABLE:Response>[${JSON.stringify({result})}]`);
            }
          }
        }
      }
      return out.join('');
    }
    catch (_) {
      return s;
    }
  }
}
*/
