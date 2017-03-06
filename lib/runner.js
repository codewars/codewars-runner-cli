// runs the code, supports both callback and Promise formats
module.exports.run = function (opts, cb) {
    // opts.code is an alias for opts.solution
    if (opts.code) opts.solution = opts.code;
    return require("./runners/" + opts.language).run(opts, cb);
};
