"use strict";

const fs = require('fs');

// mkdir -p
module.exports = function mkdirParentSync(dirPath, mode) {
  var dirs = dirPath.split("/"), partialPath;
  for (var idx = (dirPath[0] == '/' ? 1 : 0); idx < dirs.length; idx++) {
    partialPath = dirs.slice(0, idx + 1).join("/");
    if (!fs.existsSync(partialPath)) {
      fs.mkdirSync(partialPath, mode);
    }
  }
};
