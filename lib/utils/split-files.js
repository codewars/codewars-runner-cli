"use strict";

/**
 * splits a single string into multiple files. The initial content is the root content, the additional splits will
 * show up within thier own "files" object within the result object returned.
 * @param content The string to be split
 * @param regex defaults to a regex that supports `// @config: split-file File.ext`
 * @returns {{root: string, files: {}, splits: number}}
 */
module.exports = function splitFiles(content, regex = /^[ #|\/]* ?@config[: ] ?split-file (.*$)/gm) {
  const parts = content.split(regex),
        result = {root: '', files: {}, splits: 0};

  let fileName;

  parts.forEach((part, ndx) => {
    if (ndx === 0) {
      result.root = part;
    }
    else if (ndx % 2 === 1) {
      fileName = part;
    }
    else {
      // assign the content, we can remove the first character since it will be an extra line break
      result.files[fileName] = part.substr(1);
      result.splits++;
    }
  });

  return result;
};
