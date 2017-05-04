var fs = require('fs'),
    path = require('path'),
    config = require('./config'),
    child_process = require('child_process'),
    util = module.exports;

module.exports.exec = function(command, opts, cb) {
  if (!cb) {
    cb = opts;
    opts = {};
  }

  opts = opts || {};

  function handleOutput(error, stdout, stderr) {
    // wraps up any errors into a nice package and returns the json value.
    if (opts.handleError && (error || stderr)) {
      cb({
        stdout: stdout,
        stderr: [stderr, error].filter(v => !!v).join(''),
        exitCode: error && error.code,
        exitSignal: error && error.signal
      });
      return;
    }

    // if (stdout) console.log(stdout);
    if (stderr) {
      if (cb.length == 1) cb(stderr);
      else throw new Error(stderr);
    }
    else if (error !== null) {
      if (cb.length == 1) cb(error, stdout, stderr);
      else throw error;
    }
    else {
      cb();
    }
  }

  child_process.exec(command, opts, handleOutput);
};

// mkdir -p
function mkdirParentSync(dirPath, mode) {
  var dirs = dirPath.split("/"), partialPath;
  for (var idx = (dirPath[0] == '/' ? 1 : 0); idx < dirs.length; idx++) {
    partialPath = dirs.slice(0, idx + 1).join("/");
    if (!fs.existsSync(partialPath)) {
      fs.mkdirSync(partialPath, mode);
    }
  }
}

module.exports.mkdirParentSync = mkdirParentSync;

// Infer the name of a file from a module or namespace declaration in the code
function codeFileName(language, code, defaultFileName) {
  if (config.moduleRegExs[language]) {
    var fileExtensionMatch = (new RegExp('\\.' + config.fileExtensions[language] + '$')).exec(defaultFileName),
        moduleMatch = config.moduleRegExs[language].exec(code);

    if (moduleMatch !== null) {
      return moduleMatch[1].replace(/\./g, '/').replace(/-/g, '_') + '.' + config.fileExtensions[language];
    }
    if (fileExtensionMatch !== null) {
      return defaultFileName;
    }
    return defaultFileName + '.' + config.fileExtensions[language];
  }

  return defaultFileName || language;
}

module.exports.codeWriteSync = function(language, code, codeDir, defaultFileName, overwrite) {
  var fileName = codeFileName(language, code, defaultFileName);
  if (!(typeof fileName == 'string' || fileName instanceof String)) {
    throw new Error("Could not determine valid name from code:\n\n" + code);
  }

  if (!code) {
    throw new Error("Code cannot be empty!");
  }

  return util.writeFileSync(codeDir, fileName, code, overwrite);
};

module.exports.writeFileSync = function(dir, fileName, content, overwrite) {
  overwrite = overwrite !== false;
  fileName = path.join(dir || '/home/codewarrior', fileName);

  if (fs.existsSync(fileName)) {
    if (!overwrite) {
      throw new Error(["Could not write code to file ", fileName,
        " because file already exists:\n\n", content].join(""));
    }
  }

  mkdirParentSync(path.dirname(fileName));
  fs.writeFileSync(fileName, content);
  return fileName;
};

// writes multiple files. Also allows you to transform the content before it gets written. Files should be an object
// with keys mapping to file names and values mapping to file content.
module.exports.writeFilesSync = function(dir, files, overwrite, transform) {
  if (files) {
    Object.keys(files).forEach(function(fileName) {
      var content = files[fileName];
      if (transform) {
        content = transform(fileName, content);
      }

      util.writeFileSync(dir, fileName, content, overwrite);
    });
  }
};
