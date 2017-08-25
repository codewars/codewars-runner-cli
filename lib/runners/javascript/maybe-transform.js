"use strict";

const versionInfo = require('./version-info');
const transform = require('./transform');

// will babel transform the code if babel is configured, otherwise will just return the code unaltered.
module.exports = function maybeTransform(code, opts, filename) {
  const version = versionInfo(opts);
  return version.babel ? transform(code, version.name, filename) : code;
};
