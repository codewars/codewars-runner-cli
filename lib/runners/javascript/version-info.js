"use strict";

// returns information about the version passed in, such as its node version and if babel is enabled
module.exports = function versionInfo(opts) {
  const name = (opts.languageVersion || '6.x').split('/')[0];
  return {
    name: name,
    babel: opts.languageVersion && opts.languageVersion.split('/')[1] == 'babel',
    node: nodeVersion(name)
  };
};

function nodeVersion(version) {
  switch (version) {
    case "8.x":
      return "8.1.3";
    case "6.x":
    case "6.6.0": // legacy version mapping
      return "6.11.0";
    case "0.10.x":
      return "0.10.33";
    default:
      return version;
  }
}
