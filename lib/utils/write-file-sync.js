"use strict";

const path = require('path');

const fs = require('fs-extra');

module.exports = function writeFileSync(dir, fileName, content) {
  fileName = path.join(dir || '/home/codewarrior', fileName);
  fs.outputFileSync(fileName, content);
  return fileName;
};
