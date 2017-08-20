"use strict";

const createRunner = require('./create-runner');

// runs the code, supports both callback and Promise formats
module.exports.run = function(opts, cb) {
  return createRunner(require("./runners/" + opts.language))(opts, cb);
};
