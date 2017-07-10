var shovel = require('../shovel'),
    exec = require('child_process').exec,
    execSync = require('child_process').execSync,
    util = require('../util'),
    tagHelpers = require('../utils/tag-helpers'),
    manipulateFileSync = require('../utils/manipulate-file-sync'),
    fs = require('fs-extra');

module.exports.run = function run(opts, cb) {
  // we will cache the contents of this file so that we can process it later.
  let buildGradle = '';

  shovel.start(opts, cb, {
    solutionOnly: function (runCode, fail) {
      fs.emptydirSync(`${opts.dir}/src`);

      const names = extractNames(opts.solution, 'Solution');
      const solutionFile = util.codeWriteSync(null, opts.solution, `${opts.dir}/src/main/java`, names.file);

      util.codeWriteSync(null, javaMain(names.full), `${opts.dir}/src/test/java`, 'Start.java');

      buildAndTest(runCode);
    },
    testIntegration: function (runCode, fail) {

      fs.emptydirSync(`${opts.dir}/src`);
      const fixtures = [];

      if (opts.solution) {
        const solutionNames = extractNames(opts.solution, 'Solution');
        util.codeWriteSync(null, opts.solution, `${opts.dir}/src/main/java`, solutionNames.file);
      }

      if (opts.fixture) {
        const fixtureNames = extractNames(opts.fixture, 'TestFixture');
        util.codeWriteSync(null, opts.fixture, `${opts.dir}/src/main/java`, fixtureNames.file);
        fixtures.push(fixtureNames.full);
      }

      if (opts.setup) {
        const setupNames = extractNames(opts.setup);
        util.codeWriteSync(null, opts.setup, `${opts.dir}/src/main/java`, setupNames.file);
      }

      if (opts.files) {
        Object.keys(opts.files).forEach(key => {
          util.codeWriteSync(null, opts.files[key], `${opts.dir}/src/main/java`, key);

          // if there are any @Test attributes then assume test cases are included and add to the fixtures list
          if (opts.files[key].indexOf('@Test') > 0) {
            fixtures.push(extractNames(opts.files[key], key.split('.')[0]).full);
          }
        });
      }

      // setup tests, since we use a custom format we are placing the actual tests within the main folder
      // and using the test folder to wrap them.
      // TODO: make this less hacky
      util.codeWriteSync(null, javaTestRunner(fixtures), `${opts.dir}/src/test/java`, 'Start.java');

      fs.copySync('/runner/frameworks/java/src/test/java/CwRunListener.java', `${opts.dir}/src/test/java/CwRunListener.java`);

      buildAndTest(runCode);
    },
    transformBuffer: function (buffer) {
      opts.publish('processing output');
      var stdout = "", stderr = "", buildLines = [];

      // both types of streams will be embedded within the gradle output, so lets break them out
      // into their own.
      buffer.stdout.split(/^(Start > start |BUILD)/gm).forEach(line => {
        if (line.indexOf('STANDARD_OUT') == 0) {
          stdout += toStdString(line);
        }
        else if (line.indexOf('STANDARD_ERROR') == 0) {
          stderr += toStdString(line);
        }
        else {
          buildLines.push(line);
        }
      });

      // attach build output to beginning of stdout, also attach dependencies so everyone knows whats available
      // TODO: eventually we want to support this as its own buffer property, for now we are just embedding it
      // within the output in case its helpful for troubleshooting
      buildOutput = `Dependencies:\n\n${loadedDependencies()}\n\nTasks:\n\n`;
      buildOutput += buildLines.join("\n").replace(/^Start > start.*/gm, '');
      buildOutput = tagHelpers.log('-Build Output', buildOutput)
      buffer.stdout = buildOutput + stdout;

      buffer.stderr += stderr;
      // let's not show the noisy what went wrong text when there are build errors
      buffer.stderr = buffer.stderr.split(/\^* Try:/m)[0];
    }
  });

  /**
   * If the prewarming process is in-process, we want to wait until it finishes, otherwise two builds happen and that
   * will kill performance and the process will likely fail due to a timeout.
   * multiple
   * @param cb Function callback. True will be passed to the callback if the prewarm happened, which
   *                    will indicate that the deamon should have been started.
   */
  function whenReady(cb, notified) {
    const path = "/workspace/prewarm.status";
    if (!fs.pathExistsSync(path)) {
      cb(false);
    }
    else if (fs.readFileSync(path).toString().indexOf('loaded') === 0) {
      cb(true);
    }
    else {
      if (!notified) {
        opts.publish('status', 'Waiting for Gradle daemon to start...');
      }
      setTimeout(() => whenReady(cb, true), 200);
    }
  }

  // we always build and test code with the gradle test task, even if we are not actually testing anything.
  // Currently this is because we have test mode setup to embed its output within the build output.
  function buildAndTest(runCode) {
    processReferences();

    let templateOptions = {};

    // only keep the referenced dependencies, always keep junit, lombok and the other common testing libraries
    templateOptions.keeps = [{
      target: /^dependencies {\n(  .*\n)*}/gm,
      select: '^  compile.*:$keep:.*\n',
      values: ['junit', 'lombok', 'assertj-core', 'mockito-core', 'sqlite-jdbc'].concat(opts.references || [])
    }];

    buildGradle = manipulateFileSync(`/runner/frameworks/java/build.gradle`, `${opts.dir}/build.gradle`, templateOptions);

    whenReady(prewarmed => {
      runCode({
        name: 'gradle',
        // reuse the java dir cache since thats where we originally built from
        args: [
          prewarmed ? '--daemon' : '--no-daemon', // if not prewarmed don't bother
          '--stacktrace',
          '--project-cache-dir', '/runner/frameworks/java',
          // '--offline',
          '--exclude-task', 'compileGroovy',
          '--exclude-task', 'processResources',
          '--exclude-task', 'compileTestGroovy',
          '--exclude-task', 'processTestResources',
          'test'
        ],
        options: {
          cwd: opts.dir
        }
      });
    });
  }

  // checks if the spring-boot reference is loaded and adds additional settings
  function processReferences() {
    // bump the timeout to 25 seconds if spring is activated
    if ((opts.references || []).indexOf('spring-boot') >= 0) {
      opts.timeout = opts.timeout || 25000;
      opts.references.push('jackson-annotations');
      opts.references.push('spring-boot-starter-web');
      opts.references.push('spring-boot-starter-test');

      if (opts.services) {
        if (opts.services.indexOf('mongodb') >= 0) {
          opts.references.push('spring-boot-starter-data-mongodb');
        }
        if (opts.services.indexOf('redis') >= 0) {
          opts.references.push('spring-boot-starter-data-redis');
        }
        if (opts.services.indexOf('postgres') >= 0) {
          opts.references.push('spring-boot-starter-data-jpa');
        }
      }
    }
  }

  function loadedDependencies() {
    return (buildGradle.match(/  compile(?:Only)? [\'\"].*[\'\"]/g) || [])
      .map(l => l.replace(/  compile.*[\'\"](.*)[\'\"]/g, '$1')).join('\n');
  }

  // converts the embedded STDOUT and STDERR streams text to its non-embedded format
  function toStdString(text) {
    return text
      .split(/\n/)
      .filter(l => l.indexOf('    ') === 0)
      .map(l => l.replace('    ', ''))
      .join('\n') + '\n';
  }

  function extractNames (code, defaultClassName) {
    const packageName = (code.match(/\bpackage\s+([A-Z|a-z](?:[a-z|A-Z|0-9|_]|\.[A-Z|a-z])*)\W/) || [])[1];
    const className = (code.match(/\bclass\s+([A-Z][a-z|A-Z|0-9|_]*)\W/) || [])[1] || defaultClassName;

    return {
      'package': packageName,
      'class': className,
      full: packageName ? `${packageName}.${className}` : className,
      file: `${className}.java`
    };
  }

  // hacky solution for starting the dynamic main class. We need to use test mode since thats currently how we
  // have STDOUT/ERR setup to output anything
  function javaMain (mainClass) {
    const importClass = mainClass.indexOf('.') > 0 ? `import ${mainClass};` : '';
    return `
      import org.junit.Test;
      ${importClass}
      
      public class Start {
          @Test
          public void start(){
              ${mainClass}.main(null);
          }
      }
    `
  }

  // Our hacky way of running tests with our custom listener.
  // TODO: this is configurable via gradle, we just need to take the time to figure that bit out.
  function javaTestRunner (fixtures) {
    // add any imports needed, use a set to make sure we have unique values
    let imports = new Set(fixtures.filter(f => f.indexOf('.') > 0).map(f => `import ${f};\n`));
    let runs = fixtures.map(f => `runner.run(${f}.class);\n`);

    return `
      import org.junit.runner.JUnitCore;
      import org.junit.Test;
      ${[...imports].join()}
      
      public class Start {
          @Test
          public void start(){
              JUnitCore runner = new JUnitCore();
              runner.addListener(new CwRunListener());
              ${runs.join()}
          }
      }
    `;
  };
}
