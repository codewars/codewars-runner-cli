"use strict";

const expect = require('chai').expect;

const runner = require('../../runner');

describe('basics', function() {
  it('should handle basic code evaluation', function(done) {
    runner.run({
      language: 'javascript',
      code: 'console.log(42)'
    }, function(buffer) {
      expect(buffer.stdout).to.equal('42\n');
      done();
    });
  });
  it('should handle JSON.stringify ', function(done) {
    runner.run({
      language: 'javascript',
      code: 'console.log(JSON.stringify({a: 1}))'
    }, function(buffer) {
      console.log(buffer.stderr);
      expect(buffer.stdout).to.contain('{"a":1}');
      done();
    });
  });
  it('should handle importing files from a gist', function(done) {
    runner.run({
      language: 'javascript',
      code: 'console.log(require("./gist.js").name)',
      gist: '3acc7b81436ffe4ad20800e242ccaff6'
    }, function(buffer) {
      expect(buffer.stdout).to.contain('Example Test');
      done();
    });
  });

  it('should handle unicode characters', function(done) {
    runner.run({
      language: 'javascript',
      code: 'console.log("✓")'
    }, function(buffer) {
      expect(buffer.stdout).to.include("✓");
      done();
    });
  });

  it('should be able to access solution.txt', function(done) {
    runner.run({
      language: 'javascript',
      code: `
                    console.log(1+4);
                    console.log(require('fs').readFileSync('/home/codewarrior/solution.txt', 'utf8'));
                `
    }, function(buffer) {
      expect(buffer.stdout).to.contain("5");
      expect(buffer.stdout).to.contain("1+4");
      done();
    });
  });
  it('should allow a shell script to be ran', function(done) {
    runner.run({
      language: 'javascript',
      bash: 'echo "test 123" >> /home/codewarrior/test.txt ; ls',
      code: `
                    console.log(require('fs').readFileSync('/home/codewarrior/test.txt', 'utf8'));
                `
    }, function(buffer) {
      expect(buffer.stdout).to.contain("test 123");
      expect(buffer.shell.stdout.length).to.be.gt(0);
      done();
    });
  });

  // it('should be able to handle large output data', function (done) {
  //     runner.run({
  //         language: 'javascript',
  //         code: `
  //             for(i = 0;i < 9999; i++){
  //                 console.log(i * 10);
  //             }
  //         `
  //     }, function (buffer) {
  //         expect(buffer.stderr).to.equal('');
  //         done();
  //     });
  // });

  it('should handle es6 code evaluation', function(done) {
    runner.run({
      language: 'javascript',
      code: 'let a = 42; console.log(42);'
    }, function(buffer) {
      expect(buffer.stdout).to.equal('42\n');
      done();
    });
  });

  it('should handle bad babel syntax', function(done) {
    runner.run({
      language: 'javascript',
      languageVersion: '6.x/babel',
      code: 'var a = function(){returns 42;};\na();'
    }, function(buffer) {
      expect(buffer.stderr).to.contain('Unexpected token');
      done();
    });
  });

  it('should handle mongodb service with mongoose', function(done) {
    runner.run({
      language: 'javascript',
      services: ['mongodb'],
      code: `
                    var mongoose = require('mongoose');
                    mongoose.Promise = global.Promise;
                    mongoose.connect('mongodb://localhost/spec');
                    var Cat = mongoose.model('Cat', { name: String });

                    var kitty = new Cat({ name: 'Zildjian' });
                    kitty.save(function (err) {
                      if (err) {
                        console.log(err);
                      } else {
                        console.log('meow');
                      }
                      process.exit();
                    });
                `
    }, function(buffer) {
      expect(buffer.stdout).to.contain('meow');
      expect(buffer.stderr).to.be.empty;
      done();
    });
  });

  it('should handle redis service', function(done) {
    runner.run({
      language: 'javascript',
      services: ['redis'],
      code: `
                    var redis = require('redis'),
                        Promise = require('bluebird');

                    Promise.promisifyAll(redis.RedisClient.prototype);
                    var client = redis.createClient();

                    client.setAsync("foo", "bar").then(_ => {
                        client.getAsync("foo").then( v => {
                            console.log(v);
                            process.exit();
                        })
                    });
                `
    }, function(buffer) {
      expect(buffer.stdout).to.contain('bar');
      done();
    });
  });

  it('should handle react syntax', function(done) {
    runner.run({
      language: 'javascript',
      languageVersion: '6.6.0/babel',
      code: `
                    var React = require("react");
                    var ReactDOM = require("react-dom/server");
                    let render = (el) => ReactDOM.renderToStaticMarkup(el);
                    var div = <div><h3>Test</h3></div>;
                    console.log(render(div));
                `
    }, function(buffer) {
      expect(buffer.stdout).to.contain('<div><h3>Test</h3></div>');
      done();
    });
  });

  it('should handle react syntax using 0.10.x', function(done) {
    runner.run({
      language: 'javascript',
      languageVersion: '0.10.x/babel',
      code: `
                    var React = require("react");
                    var ReactDOM = require("react-dom/server");
                    let render = (el) => ReactDOM.renderToStaticMarkup(el);
                    var div = <div><h3>Test</h3></div>;
                    console.log(render(div));
                `
    }, function(buffer) {
      expect(buffer.stdout).to.contain('<div><h3>Test</h3></div>');
      done();
    });
  });

  it('should load libraries', function(done) {
    runner.run({
      language: 'javascript',
      code: 'var _ = require("lodash");console.log(_.map([1], n => n * 2));'
    }, function(buffer) {
      expect(buffer.stdout).to.contain('[ 2 ]');
      done();
    });
  });

  it('should work with SQLite', function(done) {
    runner.run({
      language: 'javascript',
      code: `
                    var sqlite3 = require('sqlite3');
                    var db = new sqlite3.Database(':memory:');
                    db.serialize(function() {
                      db.run("CREATE TABLE lorem (info TEXT)");
                      var stmt = db.prepare("INSERT INTO lorem VALUES (?)");
                      for (var i = 0; i < 10; i++) {
                          stmt.run("Ipsum " + i);
                      }
                      stmt.finalize();
                      db.each("SELECT rowid AS id, info FROM lorem", function(err, row) {
                          console.log(row.id + ": " + row.info);
                      });
                    });

                    db.close();
                `
    }, function(buffer) {
      expect(buffer.stdout).to.contain('Ipsum 0');
      expect(buffer.stdout).to.contain('Ipsum 9');
      done();
    });
  });

  it('should handle stderr', function(done) {
    runner.run({
      language: 'javascript',
      code: 'console.error("404 Not Found")'
    }, function(buffer) {
      expect(buffer.stderr).to.equal('404 Not Found\n');
      done();
    });
  });

  it('should handle stdout and stderr', function(done) {
    runner.run({
      language: 'javascript',
      code: 'console.log("stdout"); console.error("stderr")'
    }, function(buffer) {
      expect(buffer.stdout).to.equal('stdout\n');
      expect(buffer.stderr).to.equal('stderr\n');
      done();
    });
  });
});
