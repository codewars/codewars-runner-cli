"use strict";

const path = require('path');
const fs = require('fs');

const mkdirParentSync = require('./mkdir-parent-sync');

module.exports = function writeFileSync(dir, fileName, content, overwrite) {
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
