"use strict";

const config = require('../config');
const writeFileSync = require('./write-file-sync');

module.exports = function codeWriteSync(language, code, codeDir, defaultFileName) {
  var fileName = codeFileName(language, code, defaultFileName);
  if (!(typeof fileName == 'string' || fileName instanceof String)) {
    throw new Error("Could not determine valid name from code:\n\n" + code);
  }

  if (!code) throw new Error("Code cannot be empty!");

  return writeFileSync(codeDir, fileName, code);
};

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
