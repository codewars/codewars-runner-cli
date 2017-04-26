var shovel = require('../shovel'),
    codeWriteSync = require('../util').codeWriteSync,
    path = require('path'),
    temp = require('temp');

function moduleName(fileName) {
  return path.basename(fileName).replace(/\.[^/.]+$/, "");
}

function erlangCompileCommand(fileName, erlangCodeDir) {
  return [
    'compile:file("',
    fileName,
    '", {outdir,"', erlangCodeDir ,'"}),'
  ].join('');
}

function compileFileSync(code,erlangCodeDir) {
  return erlangCompileCommand(codeWriteSync('erlang', code, erlangCodeDir), erlangCodeDir);
}

module.exports.run = function(opts, cb) {
  temp.track();
  var erlangCodeDir = temp.mkdirSync('erlang');
  shovel.start(opts, cb, {
    solutionOnly: function(runCode) {
      var setup = opts.setup ? compileFileSync(opts.setup, erlangCodeDir) : '';
      runCode({
        name: 'erl',
        args: ['-pz', erlangCodeDir, '-noshell', '-eval', [setup, opts.solution].join('')],
        options: {env: {
          HOME: process.env['HOME'],
          ERL_CRASH_DUMP: "/dev/null"
        }}
      });
    },
    testIntegration: function(runCode) {
      var setup = opts.setup ? compileFileSync(opts.setup, erlangCodeDir) : '',
          solutionFileName = codeWriteSync('erlang', opts.solution, erlangCodeDir),
          solutionModuleName = moduleName(solutionFileName),
          testFixture = compileFileSync([
            '-module(' + solutionModuleName + '_tests).',
            '-compile(export_all).',
            '-include_lib("eunit/include/eunit.hrl").',
            opts.fixture
          ].join('\n'), erlangCodeDir);

      runCode({
        name: 'erl',
        args: ['-pz', erlangCodeDir, '-noshell', '-eval',
          [
            setup,
            erlangCompileCommand(solutionFileName, erlangCodeDir),
            testFixture,
            'eunit:test(', solutionModuleName, '), init:stop().'
          ].join('')],
        options: {env: {
          HOME: process.env['HOME'],
          ERL_CRASH_DUMP: "/dev/null"
        }}
      });
    }
  });
};
