"use strict";

module.exports = function transform(code, version, filename) {
  try {
    switch (version) {
      case '0.10.x':
        return require('babel-core').transform(code, {
          presets: ["stage-1", "react"],
          plugins: [
            "check-es2015-constants",
            "angular2-annotations",
            "transform-decorators-legacy",
            "transform-class-properties",
            "transform-flow-strip-types",
            "transform-es2015-arrow-functions",
            "transform-es2015-block-scoped-functions",
            "transform-es2015-block-scoping",
            "transform-es2015-classes",
            "transform-es2015-computed-properties",
            "transform-es2015-destructuring",
            "transform-es2015-duplicate-keys",
            "transform-es2015-for-of",
            "transform-es2015-function-name",
            "transform-es2015-literals",
            "transform-es2015-object-super",
            "transform-es2015-parameters",
            "transform-es2015-shorthand-properties",
            "transform-es2015-spread",
            "transform-es2015-sticky-regex",
            "transform-es2015-template-literals",
            "transform-es2015-typeof-symbol",
            "transform-es2015-unicode-regex",
            "transform-regenerator",
          ],
          ast: false,
          filename: filename || 'kata.js'
        }).code;

      default:
        return require('babel-core').transform(code, {
          presets: ["stage-1", "node5", "react"],
          plugins: [
            "angular2-annotations",
            "transform-decorators-legacy",
            "transform-class-properties",
            "transform-flow-strip-types",
          ],
          ast: false,
          filename: filename || 'kata.js'
        }).code;
    }
  }
  catch (ex) {
    var msg = ex.message;
    // if (ex.loc) {
    //     // replace the line number since it is not what the user sees
    //     msg = msg.replace(/ \(\d*:\d*\)/, ":" + ex.loc.column)
    //     var lines = code.split('\n');
    //     msg += "\n" + lines[ex.loc.line - 1];
    //     msg += "\n";
    //     for(var i = 1;i < ex.loc.column; i++) {
    //         msg += ' ';
    //     }
    //     msg += '^';
    // }
    throw new Error(msg);
  }
};
