
var util = require('util'),
    isEqual = require('underscore').isEqual,
    display = module.exports;

// prevent anyone from peeking at the code we passed in
if (global.process) {
  global.process.execArgv = null;
  global.process._eval = null;
  global.process.exit = function() {
  };

  Object.defineProperty(global.process, "_eval", {
    writable: false,
    configurable: false,
    value: "Don't cheat :)"
  });
}

function combineMessages(msgs, separator) {
  return msgs.filter(function(m) {
    return m != null;
  }).join(separator);
}

/**
 * Utility method for formatting messages.
 * @param {function|string|array} msg If a function msg will be evaluated. If an array is provided
 *  then all values will be combined with a - between them, with null values filtered out.
 * @param {string} prefix If prefix is provided it will be prepended with a - character for readability
 */
module.exports.message = function message(msg, prefix) {
  if (typeof msg == 'function') {
    msg = msg();
  }
  else if (Array.isArray(msg)) {
    msg = combineMessages(msg, ' - ');
  }
  msg = prefix ? (prefix + ' - ' + msg) : msg;
  return msg || '';
};

/**
 * Base method for writing custom output tokens.
 */
module.exports.write = function write(type, msg, opts) {
  opts = opts || {};
  var mode = (opts.mode || "").toUpperCase();
  var label = (opts.label || "");
  if (opts.mode == 'JSON') {
    msg = JSON.stringify(msg);
  }

  msg = display.format(display.message(msg));
  console.log("\n<" + type.toUpperCase() + ":" + mode + ":" + label + ">" + msg);
};

/**
 * Convenience method so that you dont have to write display.write("LOG", msg, opts) but instead display.log(msg, opts)
 */
module.exports.log = function(msg, opts) {
  display.write("LOG", msg, opts);
};

/**
 * Convenience method so that you dont have to write display.write("TAB", msg, opts) but instead display.tab(label, msg, mode)
 */
module.exports.tab = function tab(label, msg, mode) {
  display.write("TAB", msg, {label: label || "???", mode: mode});
};

/**
 * Convenience method for rendering the passed in object as JSON
 * @param obj
 * @param label
 */
module.exports.json = function json(obj, label, tab) {
  display.write(tab ? 'TAB' : 'LOG', obj, {label: label, mode: 'JSON'});
};

/**
 * Writes a propertly to the last displayed token
 */
module.exports.prop = function prop(name, value) {
  display.write("PROP", value, {label: name});
};

/**
 * renders an inspection of the object passed in. Similar to console.dir but allows the ability
 * to add a label and set as a tab
 * @param {object} obj Object to be inspected
 * @param {string} label optional label
 * @param {boolean} tab optional if it should be rendered as a tab
 */
module.exports.inspect = function inspect(obj, label, tab) {
  display.write(tab ? "TAB" : "LOG", util.inspect(obj), {label: label});
};

/**
 *  formats an value to be outputted. If a function is provided then it will be evaluated,
 *  if an object is provided then it will JSONfied. By default
 *  any line breaks will be replaced with <:BR:> so that the entire message is considered
 *  one group of data.
 */
module.exports.format = function format(obj, options) {
  options = options || {};
  var out = '';
  if (typeof obj == 'string') {
    out = obj;
  }
  else if (typeof obj == 'function') {
    out = obj.toString();
  }
  else if (obj && obj !== true) {
    // for backwards compatibility we will support the indent option
    if (options.indent || options.json) {
      out = global.Test.stringify(obj, options.indent ? 4 : 0);
    }
    else {
      out = util.inspect(obj, options);
    }
  }
  else {
    out = ('' + obj);
  }
  // replace linebreaks with LF so that they can be converted back to line breaks later. Otherwise
  // the linebreak will be treated as a new data item.
  return out.replace(/\n/g, '<:LF:>');
};


/**
 * Simple HTML escape functionality.
 */
module.exports.escapeHtml = function escapeHtml(html) {
  return String(html)
    .replace(/&/g, '&amp;')
    .replace(/"/g, '&quot;')
    .replace(/'/g, '&#39;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;');
};

/**
 * Renders a set of tabs explaining the difference between two JSON values.
 * @param expected
 * @param actual
 * @param collapsed
 */
module.exports.explainJson = function explainJson(actual, expected, collapsed) {
  display.explain(actual, expected, {collapsed: collapsed, mode: 'JSON'});
};

/**
 * Renders a set of tabs, with an optional diff tab if the values are actually different
 * @param actual
 * @param expected
 * @param options
 */
module.exports.explain = function explain(actual, expected, options) {
  // allow true to be passed in as a shortcut to setting collapsed
  if (options === true || options === false) {
    options = {collapsed: options};
  }

  options = options || {};
  var collapsed = options.collapsed ? "-" : "",
      diff = true;

  if (options.mode) {
    if (typeof(options.mode) == 'string') {
      options.mode = options.mode.toUpperCase();
    }
    // if mode is not a string, then its expected to be an explain boolean and we can throw it away.
    // doing this allows us to have pass mode strings as explain values within wrapping functions.
    // see cw-2.js Test.assertEquals
    else {
      options.mode = null;
    }
  }

  options.mode = options.mode;

  if (options.mode == 'JSON') {
    diff = actual && expected;
    // if (typeof(actual) != 'string') {
    //     actual = JSON.stringify(actual);
    // }
    //
    // if (typeof(expected) != 'string') {
    //     expected = JSON.stringify(expected);
    // }

    diff = diff && actual != expected;
  }
  // string mode is a special mode for this method which just means inspect as direct strings
  else if (options.mode == 'STRING') {
    options.mode = null;
    actual = actual ? actual.toString() : actual;
    expected = expected ? expected.toString() : expected;
    diff = expected != actual && actual && expected;
  }
  else {
    diff = !(actual && expected && isEqual(actual, expected));
    expected = util.inspect(expected);
    actual = util.inspect(actual);
  }

  // if collapsed is not specifically set, then we will only collapse if values are equal by default
  if (collapsed == null || collapsed == undefined) {
    collapsed = !diff;
  }

  display.log(expected, {label: collapsed + "Expected", mode: options.mode});

  // allows you to setup a special class for a log container
  if (options.className) {
    display.prop("className", options.className);
  }

  display.tab("Actual", actual, options.mode);

  if (diff) {
    display.tab("Diff", "", "DIFF");
  }

  if (options.arguments) {
    var details = "";
    options.arguments.forEach(function(v, i) {
      if (i > 0) details += "\n\n";
      details += "<label>Argument " + i + ":</label>\n";
      details += util.inspect(v);
    });

    display.tab("Arguments", details);
    display.prop("className", "inspection");
  }

  if (options.context) {
    display.tab("Context", util.inspect(options.context));
  }

  if (options.swap) display.write("SWAP");
};

module.exports.getPackages = function() {
  var buffer = require('child_process').execSync('npm ls --json');
  return JSON.parse(buffer.toString());
};

module.exports.availablePackages = function(label) {
  var packages = display.getPackages();
  display.json(packages, label);
};
