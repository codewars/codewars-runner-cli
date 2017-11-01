var expect = require('chai').expect;
var runner = require('../runner');
var exec = require('child_process').exec;


describe('typescript runner', function() {
  afterEach(function cleanup(done) {
    exec('rm -f /workspace/*.ts /workspace/*.js /workspace/*.txt /workspace/*.css', function(err) {
      if (err) return done(err);
      done();
    });
  });

  runner.assertCodeExamples('typescript');

  describe('.run', function() {
    it('should handle basic code evaluation', function(done) {
      runner.run({language: 'typescript', code: 'console.log(42)'}, function(buffer) {
        expect(buffer.stdout).to.equal('42\n');
        done();
      });
    });
  });

  describe('mocha bdd', function() {
    it('should handle outputting objects', function(done) {
      runner.run({
        language: 'typescript',
        code: `
                        export interface B {
                            b:number
                        };
                        export var a:B = {b: 3};
                    `,
        fixture: `
                        /// <reference path="/runner/typings/mocha/index.d.ts" />
                        /// <reference path="/runner/typings/chai/index.d.ts" />
                        import solution = require('./solution');
                        import {assert} from "chai";
                        describe("test", function(){
                            it("should be 3", function(){
                                assert.equal(3, solution.a.b);
                            })
                        });`,
        testFramework: 'mocha_bdd'
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<PASSED::>');
        done();
      });
    });
    it('should handle failures', function(done) {
      runner.run({
        language: 'typescript',
        code: 'export var a = {b: 2};',
        fixture: `
                      /// <reference path="/runner/typings/mocha/index.d.ts" />
                      /// <reference path="/runner/typings/chai/index.d.ts" />
                      import solution = require("./solution");
                      import {assert} from "chai";
                      describe("test", function(){
                        describe("failures", function(){
                          it("should be 1", function(){
                            assert.equal(1, solution.a.b);
                        })
                      })
                    });`,
        testFramework: 'mocha_bdd'
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<FAILED::>');
        done();
      });
    });
    it('should handle errors', function(done) {
      runner.run({
        language: 'typescript',
        code: 'export var a = {b: 2};',
        fixture: `
                      /// <reference path="/runner/typings/mocha/index.d.ts" />
                      /// <reference path="/runner/typings/chai/index.d.ts" />
                      import solution = require("./solution");
                      import {assert} from "chai";
                      describe("test", function(){
                        describe("failures", function(){
                            it("should be 1", function(){
                                throw new Error("test error");
                            })
                        })
                      });
                    `,
        testFramework: 'mocha_bdd'
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<ERROR::>');
        done();
      });
    });

    it('should have formatting commands on independent lines', function(done) {
      runner.run({
        language: 'typescript',
        solution: `//`,
        fixture: [
          `/// <reference path="/runner/typings/node/index.d.ts" />`,
          `/// <reference path="/runner/typings/mocha/index.d.ts" />`,
          `/// <reference path="/runner/typings/chai/index.d.ts" />`,
          `import {assert} from "chai";`,
          ``,
          `describe("tests", function() {`,
          `  it("test", function() {`,
          `    process.stdout.write('foo');`,
          `    assert.equal(1, 1);`,
          `  });`,
          `});`,
        ].join('\n'),
        testFramework: 'mocha_bdd',
      }, function(buffer) {
        expect(buffer.stdout).to.contain('foo\n<PASSED::>Passed\n');
        done();
      });
    });
  });


  //----------------------------------------------------------------------------------------
  // Karma BDD
  //----------------------------------------------------------------------------------------

  describe('karma bdd', function() {
    it('basic test', function(done) {
      runner.run({
        language: 'typescript',
        languageVersion: '2.4',
        code: 'export var a = {b: 2};',
        fixture: `\
          /// <reference path="/runner/typings/mocha/index.d.ts" />
          /// <reference path="/runner/typings/chai/index.d.ts" />
          import 'core-js';
          import {assert} from 'chai';
          import {a} from './solution';
          describe("test", function(){
            it("should be 2", function(){
              assert.equal(2, a.b);
            });
          });`,
        testFramework: 'karma_bdd'
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<PASSED::>');
        done();
      });
    });
    describe('Angular 4', function() {
      it('handle successes', function(done) {
        runner.run({
          language: 'typescript',
          languageVersion: '2.4',
          code: `\
            import 'core-js';
            import { Component } from "@angular/core";

            @Component({
                selector: "app-hello",
                template: "<h1>{{title}}</h1>"
            })
            export class HelloComponent {
                public title = "Hello :)";
            }`,
          setup: `\
            import 'core-js';
            import 'zone.js/dist/zone';
            import 'zone.js/dist/long-stack-trace-zone';
            import 'zone.js/dist/proxy';
            import 'zone.js/dist/sync-test';
            import 'zone.js/dist/mocha-patch';
            import 'zone.js/dist/async-test';
            import 'zone.js/dist/fake-async-test';

            import { TestBed } from '@angular/core/testing';
            import { BrowserDynamicTestingModule, platformBrowserDynamicTesting } from '@angular/platform-browser-dynamic/testing';

            TestBed.initTestEnvironment(BrowserDynamicTestingModule, platformBrowserDynamicTesting());
          `,
          fixture: `\
            /// <reference path="/runner/typings/mocha/index.d.ts" />
            /// <reference path="/runner/typings/chai/index.d.ts" />
            import { expect } from 'chai';
            import { DebugElement } from "@angular/core";
            import { async, ComponentFixture, TestBed } from "@angular/core/testing";
            import { By } from "@angular/platform-browser";

            import {HelloComponent} from './solution';

            describe("HelloComponent", () => {

                let fixture: ComponentFixture<HelloComponent>;

                beforeEach(async(() => {

                    return TestBed
                        .configureTestingModule({
                            declarations: [HelloComponent]
                        })
                        .compileComponents()
                        .then(() => {
                            fixture = TestBed.createComponent(HelloComponent);
                        });
                }));

                it("should display original title", () => {

                    let debugElement = fixture.debugElement.query(By.css("h1"));
                    fixture.detectChanges();

                    expect(debugElement.nativeElement.textContent).to.equal("Hello :)");
                });

                it("should display a different test title", () => {

                    let debugElement = fixture.debugElement.query(By.css("h1"));

                    fixture.componentInstance.title = "Test Title";
                    fixture.detectChanges();

                    expect(debugElement.nativeElement.textContent).to.equal("Test Title");
                });
            });`,
          testFramework: 'karma_bdd'
        }, function(buffer) {
          expect(buffer.stdout).to.contain('<IT::>should display original title');
          expect(buffer.stdout).to.contain('<PASSED::>');
          done();
        });
      });
      it('handles failure', function(done) {
        runner.run({
          language: 'typescript',
          languageVersion: '2.4',
          code: `\
            import 'core-js';
            import { Component } from "@angular/core";

            @Component({
                selector: "app-hello",
                template: "<h1>{{tittle}}</h1>"
            })
            export class HelloComponent {
                public title = "Hello :)";
            }`,
          setup: `\
            import 'core-js';
            import 'zone.js/dist/zone';
            import 'zone.js/dist/long-stack-trace-zone';
            import 'zone.js/dist/proxy';
            import 'zone.js/dist/sync-test';
            import 'zone.js/dist/mocha-patch';
            import 'zone.js/dist/async-test';
            import 'zone.js/dist/fake-async-test';

            import { TestBed } from '@angular/core/testing';
            import { BrowserDynamicTestingModule, platformBrowserDynamicTesting } from '@angular/platform-browser-dynamic/testing';

            TestBed.initTestEnvironment(BrowserDynamicTestingModule, platformBrowserDynamicTesting());
          `,
          fixture: `\
            /// <reference path="/runner/typings/mocha/index.d.ts" />
            /// <reference path="/runner/typings/chai/index.d.ts" />
            import { expect } from 'chai';
            import { DebugElement } from "@angular/core";
            import { async, ComponentFixture, TestBed } from "@angular/core/testing";
            import { By } from "@angular/platform-browser";

            import {HelloComponent} from './solution';

            describe("HelloComponent", () => {

                let fixture: ComponentFixture<HelloComponent>;

                beforeEach(async(() => {

                    return TestBed
                        .configureTestingModule({
                            declarations: [HelloComponent]
                        })
                        .compileComponents()
                        .then(() => {
                            fixture = TestBed.createComponent(HelloComponent);
                        });
                }));

                it("should display original title", () => {

                    let debugElement = fixture.debugElement.query(By.css("h1"));
                    fixture.detectChanges();

                    expect(debugElement.nativeElement.textContent).to.equal("Hello :)");
                });
            });`,
          testFramework: 'karma_bdd'
        }, function(buffer) {
          expect(buffer.stdout).to.contain('<FAILED::>');
          done();
        });
      });
    });
  });


  //----------------------------------------------------------------------------------------
  // Karma TDD
  //----------------------------------------------------------------------------------------

  describe('karma tdd', function() {
    it('basic test', function(done) {
      runner.run({
        language: 'typescript',
        languageVersion: '2.4',
        code: 'export var a = {b: 2};',
        fixture: `\
          /// <reference path="/runner/typings/mocha/index.d.ts" />
          /// <reference path="/runner/typings/chai/index.d.ts" />
          import 'core-js';
          import {assert} from 'chai';
          import {a} from './solution';
          suite("test", function(){
            test("should be 2", function(){
              assert.equal(2, a.b);
            });
          });`,
        testFramework: 'karma_tdd'
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<PASSED::>');
        done();
      });
    });
    describe('Angular 4', function() {
      it('handle successes', function(done) {
        runner.run({
          language: 'typescript',
          languageVersion: '2.4',
          code: `\
            import 'core-js';
            import { Component } from "@angular/core";

            @Component({
                selector: "app-hello",
                template: "<h1>{{title}}</h1>"
            })
            export class HelloComponent {
                public title = "Hello :)";
            }`,
          setup: `\
            import 'core-js';
            import 'zone.js/dist/zone';
            import 'zone.js/dist/long-stack-trace-zone';
            import 'zone.js/dist/proxy';
            import 'zone.js/dist/sync-test';
            import 'zone.js/dist/mocha-patch';
            import 'zone.js/dist/async-test';
            import 'zone.js/dist/fake-async-test';

            import { TestBed } from '@angular/core/testing';
            import { BrowserDynamicTestingModule, platformBrowserDynamicTesting } from '@angular/platform-browser-dynamic/testing';

            TestBed.initTestEnvironment(BrowserDynamicTestingModule, platformBrowserDynamicTesting());
          `,
          fixture: `\
            /// <reference path="/runner/typings/mocha/index.d.ts" />
            /// <reference path="/runner/typings/chai/index.d.ts" />
            import { expect } from 'chai';
            import { DebugElement } from "@angular/core";
            import { async, ComponentFixture, TestBed } from "@angular/core/testing";
            import { By } from "@angular/platform-browser";

            import {HelloComponent} from './solution';

            suite("HelloComponent", () => {

                let fixture: ComponentFixture<HelloComponent>;

                beforeEach(async(() => {

                    return TestBed
                        .configureTestingModule({
                            declarations: [HelloComponent]
                        })
                        .compileComponents()
                        .then(() => {
                            fixture = TestBed.createComponent(HelloComponent);
                        });
                }));

                test("should display original title", () => {

                    let debugElement = fixture.debugElement.query(By.css("h1"));
                    fixture.detectChanges();

                    expect(debugElement.nativeElement.textContent).to.equal("Hello :)");
                });

                test("should display a different test title", () => {

                    let debugElement = fixture.debugElement.query(By.css("h1"));

                    fixture.componentInstance.title = "Test Title";
                    fixture.detectChanges();

                    expect(debugElement.nativeElement.textContent).to.equal("Test Title");
                });
            });`,
          testFramework: 'karma_tdd'
        }, function(buffer) {
          expect(buffer.stdout).to.contain('<IT::>should display original title');
          expect(buffer.stdout).to.contain('<PASSED::>');
          done();
        });
      });
      it('handles failure', function(done) {
        runner.run({
          language: 'typescript',
          languageVersion: '2.4',
          code: `\
            import 'core-js';
            import { Component } from "@angular/core";

            @Component({
                selector: "app-hello",
                template: "<h1>{{tittle}}</h1>"
            })
            export class HelloComponent {
                public title = "Hello :)";
            }`,
          setup: `\
            import 'core-js';
            import 'zone.js/dist/zone';
            import 'zone.js/dist/long-stack-trace-zone';
            import 'zone.js/dist/proxy';
            import 'zone.js/dist/sync-test';
            import 'zone.js/dist/mocha-patch';
            import 'zone.js/dist/async-test';
            import 'zone.js/dist/fake-async-test';

            import { TestBed } from '@angular/core/testing';
            import { BrowserDynamicTestingModule, platformBrowserDynamicTesting } from '@angular/platform-browser-dynamic/testing';

            TestBed.initTestEnvironment(BrowserDynamicTestingModule, platformBrowserDynamicTesting());
          `,
          fixture: `\
            /// <reference path="/runner/typings/mocha/index.d.ts" />
            /// <reference path="/runner/typings/chai/index.d.ts" />
            import { expect } from 'chai';
            import { DebugElement } from "@angular/core";
            import { async, ComponentFixture, TestBed } from "@angular/core/testing";
            import { By } from "@angular/platform-browser";

            import {HelloComponent} from './solution';

            suite("HelloComponent", () => {

                let fixture: ComponentFixture<HelloComponent>;

                beforeEach(async(() => {

                    return TestBed
                        .configureTestingModule({
                            declarations: [HelloComponent]
                        })
                        .compileComponents()
                        .then(() => {
                            fixture = TestBed.createComponent(HelloComponent);
                        });
                }));

                test("should display original title", () => {

                    let debugElement = fixture.debugElement.query(By.css("h1"));
                    fixture.detectChanges();

                    expect(debugElement.nativeElement.textContent).to.equal("Hello :)");
                });
            });`,
          testFramework: 'karma_tdd'
        }, function(buffer) {
          expect(buffer.stdout).to.contain('<FAILED::>');
          done();
        });
      });
    });
  });

  describe('targets', function() {
    it('should support running with ES6 target', function(done) {
      runner.run({
        language: 'typescript',
        languageVersion: '2.4/ES6',
        solution: [
          `class Cube {`,
          `  private _n: number;`,
          `  constructor(n) {`,
          `    this._n = n;`,
          `  }`,
          `  get volume(): number { return Math.pow(this._n, 3); }`,
          `}`,
          ``,
          `const c = new Cube(2);`,
          `console.log(c.volume);`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.equal('8\n');
        done();
      });
    });

    it('should default to ES3', function(done) {
      runner.run({
        language: 'typescript',
        solution: [
          `class Cube {`,
          `  private _n: number;`,
          `  constructor(n) {`,
          `    this._n = n;`,
          `  }`,
          `  get volume(): number { return Math.pow(this._n, 3); }`,
          `}`,
          ``,
          `const c = new Cube(2);`,
          `console.log(c.volume);`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).contains('Accessors are only available when targeting ECMAScript 5 and higher.');
        done();
      });
    });

    it('should support testing with ES6 target', function(done) {
      runner.run({
        language: 'typescript',
        languageVersion: '2.4/ES6',
        testFramework: 'mocha_bdd',
        solution: [
          `export class Cube {`,
          `  private _n: number;`,
          `  constructor(n) {`,
          `    this._n = n;`,
          `  }`,
          `  get volume(): number { return Math.pow(this._n, 3); }`,
          `}`,
        ].join('\n'),
        fixture: [
          `/// <reference path="/runner/typings/mocha/index.d.ts" />`,
          `/// <reference path="/runner/typings/chai/index.d.ts" />`,
          `import {Cube} from "./solution";`,
          `import {assert} from "chai";`,
          `describe("Cube", function() {`,
          `  it("should have volume getter", function() {`,
          `    assert.equal(new Cube(2).volume, 8);`,
          `  });`,
          `});`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<PASSED::>Passed');
        done();
      });
    });
  });

  describe('typings', function() {
    it('should support the reference path under /runner/typings/main/ambient', function(done) {
      runner.run({
        language: 'typescript',
        code: `
                        export interface B {
                            b:number
                        };
                        export var a:B = {b: 3};
                    `,
        fixture: `
                        /// <reference path="/runner/typings/main/ambient/mocha/index.d.ts" />
                        /// <reference path="/runner/typings/main/ambient/chai/index.d.ts" />
                        import solution = require('./solution');
                        import {assert} from "chai";
                        describe("test", function(){
                            it("should be 3", function(){
                                assert.equal(3, solution.a.b);
                            })
                        });`,
        testFramework: 'mocha_bdd'
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<PASSED::>');
        done();
      });
    });
  });
});
