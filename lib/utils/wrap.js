"use strict";

module.exports = function wrap(value) {
  return Array.isArray(value) ? value : [value];
};
