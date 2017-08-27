"use strict";

const splitFiles = require('./utils/split-files.js');

module.exports = function processOptions(opts) {
  opts = assignConfigJson(opts);

  // set the default directory to run everything out of
  opts.dir = opts.dir || '/home/codewarrior';

  // opts.code is an alias for opts.solution
  if (opts.code) {
    opts.solution = opts.code;
  }

  assignProjectMode(opts);

  assignEntryFile(opts);

  assignPublishing(opts);

  assignBashFile(opts);

  assignConfigStatements(opts);

  assignStrategy(opts);

  // in case anything needs to cleanup after itself
  opts.onCompleted = [];

  opts.services = opts.services || [];

  return opts;
};

function assignBashFile(opts) {
  if (!opts.bashFile && opts.files && opts.files['.runner/setup.sh']) {
    opts.bashFile = '.runner/setup.sh';
  }
}

// project mode is when multiple files have been provided and not a solution value, which indicates
// that an entry file is to be specified and that the format is determined by the configuration.
function assignProjectMode(opts) {
  // if empty files were passed in, make the value null to make it easier to check for emptyness
  if (opts.files && Object.keys(opts.files).length === 0) {
    opts.files = null;
  }

  // indicate if we should process the files. If a solution is provided, then we assume the files are just there
  // as extra content, if no solution is provided, then one of the files will be treated as the entry file.
  if (!opts.solution && opts.files) {
    opts.projectMode = true;
  }
}

// determines the file to be used as the entry file when project mode is enabled
function assignEntryFile(opts) {
  if (opts.projectMode) {
    if (opts.entryFile) {
      // if there is an entry file lets see if we need to convert it from a wildcard
      if (opts.entryFile.indexOf('*') >= 0) {
        opts.entryFile = findWildcardFile(opts, opts.entryFile);
      }
    }

    // if no entry file was specified (or was not found using the wildcard above)
    if (!opts.entryFile) {
      // loop through some common entry points and try to find a match
      for (var search of ['spec.*', 'test.*', 'fixture.*', 'entry.*']) {
        opts.entryFile = findWildcardFile(opts, search);
        if (opts.entryFile) break;
      }
    }

    if (opts.entryFile) {
      opts.entryPath = `${opts.dir}/${opts.entryFile}`;
    }

    // setup file paths so its easier to include them
    opts.filePaths = Object.keys(opts.files).map(name => `${opts.dir}/${name}`);
    opts.filteredFilePaths = function(ext) {
      var re = new RegExp(`\.${ext}$`);
      return opts.filePaths.filter(p => !!p.match(re));
    };
  }
}

function findWildcardFile(opts, search) {
  for (var fileName in opts.files) {
    if (wildcardMatch(fileName, search)) {
      return fileName;
    }
  }
}

function wildcardMatch(str, search) {
  if (!str) return str;
  return !!str.match(new RegExp(search.replace(/\./g, "\\.").replace(/\*/g, '.*')));
}

// if a specific strategy is not provided then determine based off of the data passed in
function assignStrategy(opts) {
  if (!opts.strategy) {
    // if a fixture is provided or in project mode
    if (opts.projectMode || opts.fixture) {
      opts.strategy = 'testIntegration';
    }
    else {
      opts.strategy = 'solutionOnly';
    }
  }
}

function assignConfigJson(opts) {
  // if a specific config file was uploaded, then use that as well
  if (opts.files && opts.files['.runner/config.json']) {
    // we want to use it as the defaults though, any specific options passed in will override the config file
    return Object.assign(opts, JSON.parse(opts.files['.runner/config.json']));
  }

  return opts;
}

function assignPublishing(opts) {
  opts.publish = function() {};

  if (opts.ably && opts.channel) {
    try {
      var ably = new require('ably').Rest(opts.ably, {log: {level: 0}});
      var channel = ably.channels.get(opts.channel);
      opts.publish = function(event, data) {
        if (event && data) {
          channel.publish(event, data);
        }
      };
    }
    catch (e) {} // eslint-disable-line no-empty
  }
}

// we allow configuration to be applied via the setup code block. This is useful for requests in non-project mode.
// project mode configuration would normally be done instead via adding a .runner/config.json file.
function assignConfigStatements(opts) {
  let match;

  if (opts.setup) {
    // bashFile will override any default config settings for a script to run
    match = opts.setup.match(/^[ #|\/]* ?@config: bash-file (.*$)/m);
    if (match) opts.bashFile = match[1];

    // bash will override any default config settings for a script to run
    match = opts.setup.match(/^[ #|\/]* ?@config: bash (.*$)/m);
    if (match) opts.bash = match[1];

    // githubRepo
    match = opts.setup.match(/^[ #|\/]* ?@config: github-repo (.*$)/m);
    if (match) opts.githubRepo = match[1];

    // gist
    match = opts.setup.match(/^[ #|\/]* ?@config: gist (.*$)/m);
    if (match) opts.gist = match[1];

    // solutionPath - indcates to some languages what the solution file should be called
    match = opts.setup.match(/^[ #|\/]* ?@config: solution-path (.*$)/m);
    if (match) opts.solutionPath = match[1];

    // services - indcates which services should be enabled. commas seperated (ie: mongodb,redis)
    match = opts.setup.match(/^[ #|\/]* ?@config: services (.*$)/m);
    if (match) opts.services = match[1].split(',');

    match = opts.setup.match(/@(?:config: )?include-external\s+\S+/g);
    if (match) {
      opts.externalIncludes = opts.externalIncludes || [];
      match.forEach(match => {
        let name = match.replace(/@(?:config: )?include-external\s+/, '');
        opts.externalIncludes.push(name);
      });
    }

    match = opts.setup.match(/@(?:config: )?reference\s+\S+/g);
    if (match) {
      opts.references = opts.references || [];
      match.forEach(match => {
        let name = match.replace(/@(?:config: )?reference\s+/, '');
        opts.references.push(name);
      });
    }
  }

  assignSplits(opts, 'setup');
  assignSplits(opts, 'solution');
  assignSplits(opts, 'fixture');
}

function assignSplits(opts, field) {
  if (opts[field]) {
    let fileSplits = splitFiles(opts[field]);
    if (fileSplits.splits) {
      opts[field] = fileSplits.root;
      opts.files = Object.assign(opts.files || {}, fileSplits.files);
    }
  }
}
