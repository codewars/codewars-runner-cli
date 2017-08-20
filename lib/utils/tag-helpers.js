"use strict";

module.exports.log = function log(label, value) {
  return `<LOG::${label}>${(value || '').replace(/\n/g, "<:LF:>")}\n`;
};
