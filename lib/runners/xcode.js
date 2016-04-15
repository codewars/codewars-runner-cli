var shovel = require("../shovel"),
codeWriteSync = require('../util').codeWriteSync,
_ = require("lodash"),
path = require("path"),
temp = require("temp");

module.exports.fullProject = function fullProject(opts,run) {
    temp.track();
    var tempCodeDir = temp.mkdirSync("xcode"),
        script = path.resolve(__dirname, "../../frameworks/osx/run_template.sh"),
        files = {"CODE": codeWriteSync(opts.language, opts.code, tempCodeDir, "code"),
                 "FIXTURE": codeWriteSync(opts.language, opts.fixture, tempCodeDir, "test")};
    if (opts.language == "objc")
        files.CODE_HEADER = codeWriteSync("objcHeader", opts.codeHeader, tempCodeDir, "code.h");
    if ("setup" in opts) {
        files.SETUP = codeWriteSync(opts.language, opts.setup, tempCodeDir, "setup");
        if (opts.language == "objc")
            files.SETUP_HEADER = codeWriteSync("objcHeader", opts.setupHeader, tempCodeDir, "setup.h");
    }
    run({ name: script,
          options: {"env": _.merge({}, process.env, opts, files)}});};
