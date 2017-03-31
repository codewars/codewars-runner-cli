// this file is just here for backwards compatibility since python 3 used to be considered its own language.
var python = require('./python');

module.exports.run = function run(opts, cb) {
  opts.languageVersion = '3.x'; // we only care about the major version
  python.run(opts, cb);
};


