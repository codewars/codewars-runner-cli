"use strict";

const path = require('path');
const fs = require('fs-extra');

module.exports = function writeFileSync(dir, fileName, content, overwrite) {
  fileName = path.join(dir || '/home/codewarrior', fileName);
  if (overwrite === false && fs.existsSync(fileName)) {
    throw new Error(`Could not write code to file ${fileName} because file already exists:\n\n${content}`);
  }
  fs.outputFileSync(fileName, content);
  return fileName;
};
