"use strict";

const fs = require('fs-extra');

const wrap = require('./wrap');

/**
 * Takes the content of the source file and moves them to the dest, allowing you to manipulate the contents first.
 * @param src A string representing the source path
 * @param dest A string for representing the destination path
 * @param options A required object of settings:
 *    transforms: A function or an array of transform functions
 *    replacements: An array of replacement definitions
 *    keeps: An Array of keep definitions
 *    write: writeFileSync options
 *
 */
module.exports = function manipulateFileSync(src, dest, options) {
  let content = fs.readFileSync(src, 'utf8');

  // transforms are raw methods which will transform the content
  if (options.transforms) {
    wrap(options.transforms).forEach(t => content = t(content));
  }

  // replacements are objects which indicate how a content should be replaced.
  if (options.replacements) {
    wrap(options.replacements).forEach(r => content = replace(content, r));
  }

  // keeps are objects which indicate which content should be kept
  if (options.keeps) {
    wrap(options.keeps).forEach(k => content = keep(content, k));
  }

  fs.outputFileSync(dest, content, options.write);

  return content;
};

/**
 * Instead of replacing content, this method tries to determine which content should be kept
 * @param content String
 * @param options Object
 *    target: a regexp that determines which content should be targetted, if only a subset should be eligible
 *    select: a string regexp that determines how to select content. $keep is a special token used to indicate how keep values are included
 *    values: an array of strings representing capture group values
 * @returns String transformed content
 */
function keep(content, options) {
  const targets = options.target ? content.match(options.target) : null;
  let targetContent = targets && targets.length ? targets[0] : content;

  if (targetContent) {
    const keepsRegex = new RegExp(options.select.replace('$keep', `(${options.values.join('|')})`), 'gm');
    const cleanRegex = new RegExp(options.select.replace('$keep', '.*'), 'gm');

    const keepLines = targetContent.match(keepsRegex);
    const insertIndex = targetContent.search(cleanRegex);
    targetContent = insert(targetContent.replace(cleanRegex, ''), insertIndex, `${keepLines.join(options.join || '')}\n`);

    content = options.target ? content.replace(options.target, targetContent) : targetContent;
  }

  return content;
}

/**
 * Replaces part of the content
 * @param content String
 * @param options Object
 *    replace: String or Regex that must be provided to determine how to replace
 *    content: String to use as the replacement content
 *    file: String of the file path to the file to be used as content for the replacement
 *    encoding: String the encoding of the file
 */
function replace(content, options) {
  let substitute = options.content || '';
  if (options.file) {
    substitute = fs.readFileSync(options.file, options.encoding || 'utf8');
  }

  return content.replace(options.replace, substitute);
}

function insert(str, index, value) {
  return str.substr(0, index) + value + str.substr(index);
}
