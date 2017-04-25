// runs the code, supports both callback and Promise formats
module.exports.run = function(opts, cb) {
  return require("./runners/" + opts.language).run(opts, cb);
};
