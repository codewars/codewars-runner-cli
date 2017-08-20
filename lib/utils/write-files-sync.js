"use strict";

const writeFileSync = require('./write-file-sync');

// writes multiple files. Also allows you to transform the content before it gets written. Files should be an object
// with keys mapping to file names and values mapping to file content.
module.exports = function writeFilesSync(dir, files, transform) {
  if (files) {
    Object.keys(files).forEach(function(fileName) {
      var content = files[fileName];
      if (transform) content = transform(fileName, content);
      writeFileSync(dir, fileName, content);
    });
  }
};
