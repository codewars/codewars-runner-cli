var shovel = require('../shovel'),
    writeFileSync = require('../util').writeFileSync;

module.exports.run = function run(opts, cb) {
    const dir = '/home/codewarrior/lua';
    shovel.start(opts, cb, {
        solutionOnly: function(runCode) {
            runCode({
              name: 'lua',
              args: [writeFileSync(dir, 'solution.lua', opts.solution, true)]
            });
        },
        testIntegration: function(runCode) {
            writeFileSync(dir, 'solution.lua', opts.solution, true);
            runCode({
              name: 'busted',
              args: [
                writeFileSync(dir, 'fixture.lua', opts.fixture, true),
                `--output=codewars.lua`,
              ],
              options: {cwd: dir}
            });
        }
    });
};
