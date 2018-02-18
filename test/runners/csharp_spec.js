"use strict";

const expect = require('chai').expect;

const runner = require('../runner');

describe('c# runner', function() {
  describe('.run', function() {
    runner.assertCodeExamples('csharp');

    it('should handle basic code evaluation', function(done) {
      runner.run({
        language: 'csharp',
        solution: [
          'public class Hello1 {',
          '  public static void Main() {',
          '    System.Console.WriteLine("Hello, World!");',
          '  }',
          '}',
        ].join('\n'),
      }, function(buffer) {
        //console.log(buffer);
        expect(buffer.stdout).to.contain('Hello, World!\n');
        done();
      });
    });

    it('should handle setup', function(done) {
      runner.run({
        language: 'csharp',
        setup: [
          'public class Preloaded {',
          '  public static string hello = "Hello, World!";',
          '}',
        ].join('\n'),
        solution: [
          'public class Hello1 {',
          '  public static void Main() {',
          '    System.Console.WriteLine(Preloaded.hello);',
          '  }',
          '}',
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.contain('Hello, World!\n');
        done();
      });
    });

    it('should handle basic nunit tests', function(done) {
      runner.run({
        language: 'csharp',
        solution: [
          'namespace Bank {',
          '  using System;',
          '  public class Account {',
          '    private decimal balance;',
          '    public void Deposit(decimal amount) {',
          '      Console.WriteLine("slorgs");',
          '      balance += amount;',
          '    }',
          '    public void Withdraw(decimal amount) {',
          '      balance -= amount;',
          '    }',
          '    public void TransferFunds(Account destination, decimal amount) {',
          '    }',
          '    public decimal Balance {',
          '      get { return balance; }',
          '    }',
          '  }',
          '} ',
        ].join('\n'),
        fixture: [
          'namespace Bank {',
          '  using NUnit.Framework;',
          '  [TestFixture]',
          '  public class AccountTest {',
          '    [Test]',
          '    public void TransferFunds() {',
          '      Account source = new Account();',
          '      source.Deposit(200m);',
          '      Account destination = new Account();',
          '      destination.Deposit(150m);',
          '      source.TransferFunds(destination, 100m);',
          '      Assert.AreEqual(250m, destination.Balance);',
          '      Assert.AreEqual(100m, source.Balance);',
          '    }',
          '    [Test]',
          '    public void CheckFunds() {',
          '      Account source = new Account();',
          '      source.Deposit(200m);',
          '      Account destination = new Account();',
          '      destination.Deposit(150m);',
          '      Assert.AreEqual(200m, source.Balance);',
          '    }',
          '  }',
          '}',
        ].join('\n'),
      }, function(buffer) {
        //console.log(buffer);
        expect(buffer.stdout).to.contain("slorgs");
        expect(buffer.stdout).to.contain("<PASSED::>");
        expect(buffer.stdout).to.contain("<FAILED::>");
        expect(buffer.stdout).to.contain("<DESCRIBE::>");
        expect(buffer.stdout).to.contain("<IT::>");
        done();
      });
    });

    it('should handle bad code', function(done) {
      runner.run({
        language: 'csharp',
        solution: [
          'namespace Bank {',
          '  using System;',
          '  using System.Drawing;',
          '  public class Account {',
          '    private decimal balance;',
          '    public void Deposit(decimal amount) {',
          '      Console.WriteLine("slorgs");',
          '      balance += amount;',
          '    }',
          '    public void Withdraw(decimal amount) {',
          '      balance -= Amount;',
          '    }',
          '    public void TransferFunds(Account destination, decimal amount) { }',
          '    public decimal Balance { get { return balance; } }',
          '  }',
          '}',
        ].join('\n'),
        fixture: [
          'namespace Bank {',
          '  using NUnit.Framework;',
          '  [TestFixture]',
          '  public class AccountTest {',
          '    [Test]',
          '    public void TransferFunds() {',
          '      Account source = new Account();',
          '      source.Deposit(200m);',
          '      Account destination = new Account();',
          '      destination.Deposit(150m);',
          '      source.TransferFunds(destination, 100m);',
          '      Assert.AreEqual(250m, destination.Balance);',
          '      Assert.AreEqual(100m, source.Balance);',
          '    }',
          '    [Test]',
          '    public void CheckFunds() {',
          '      Account source = new Account();',
          '      source.Deposit(200m);',
          '      Account destination = new Account();',
          '      destination.Deposit(150m);',
          '      Assert.AreEqual(200m, source.Balance);',
          '    }',
          '  }',
          '}',
        ].join('\n'),
      }, function(buffer) {
        console.log(buffer);
        expect(buffer.stdout).to.not.contain("lib/runners/csharp.js");
        expect(buffer.stderr).to.contain("does not exist in the current context");
        done();
      });
    });

    it('should handle compilation errors with clean output', function(done) {
      runner.run({
        language: 'csharp',
        format: 'json',
        solution: [
          'public class CheckChoose {',
          '  public static void Main() {}',
          '  public static long Checkchoose(long m, int n) {}',
          '}',
        ].join('\n'),
      }, function(buffer) {
        // console.log(buffer);
        console.log('');
        expect(buffer.stdout).to.not.contain('stdout');
        expect(buffer.stderr).to.not.contain('stdout');
        expect(buffer.stderr).to.not.equal('');
        done();
      });
    });

    it('should handle partially passed code', function(done) {
      runner.run({
        language: 'csharp',
        solution: [
          'using System;',
          'public class Account { }',
        ].join('\n'),
        fixture: [
          'using NUnit.Framework;',
          '[TestFixture]',
          'public class AccountTest {',
          '  [Test]',
          '  public void A() {',
          '    Assert.AreEqual(1, 1);',
          '  }',
          '  [Test]',
          '  public void Z() {',
          '    Assert.AreEqual(1, 1);',
          '    throw new System.Exception();',
          '  }',
          '}',
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.contain("<ERROR::>");
        done();
      });
    });

    it('should handle project mode', function(done) {
      runner.run({
        language: 'csharp',
        // references: ['SkiaSharp.dll'],
        files: {
          'foo.sh': 'test',
          'solution.cs': [
            'namespace Bank {',
            '  using System;',
            '  public class Account {',
            '    private decimal balance;',
            '    public void Deposit(decimal amount) {',
            '      Console.WriteLine("slorgs");',
            '      balance += amount;',
            '    }',
            '    public void Withdraw(decimal amount) { balance -= amount; }',
            '    public void TransferFunds(Account destination, decimal amount) { }',
            '    public decimal Balance { get { return balance; } }',
            '  }',
            '}',
          ].join('\n'),
          'spec.cs': [
            'namespace Bank {',
            '  using NUnit.Framework;',
            '  [TestFixture]',
            '  public class AccountTest {',
            '    [Test]',
            '    public void TransferFunds() {',
            '      Account source = new Account();',
            '      source.Deposit(200m);',
            '      Account destination = new Account();',
            '      destination.Deposit(150m);',
            '      source.TransferFunds(destination, 100m);',
            '      Assert.AreEqual(250m, destination.Balance);',
            '      Assert.AreEqual(100m, source.Balance);',
            '    }',
            '    [Test]',
            '    public void CheckFunds() {',
            '      Account source = new Account();',
            '      source.Deposit(200m);',
            '      Account destination = new Account();',
            '      destination.Deposit(150m);',
            '      Assert.AreEqual(200m, source.Balance);',
            '    }',
            '  }',
            '}',
          ].join('\n'),
        }
      }, function(buffer) {
        //console.log(buffer);
        expect(buffer.stdout).to.contain("slorgs");
        expect(buffer.stdout).to.contain("<PASSED::>");
        done();
      });

      it('should have output format commands on independent lines', function(done) {
        runner.run({
          language: 'csharp',
          solution: [
            'public class Solution {}',
          ].join('\n'),
          fixture: [
            `using System;`,
            `using NUnit.Framework;`,
            ``,
            `[TestFixture]`,
            `public class KataTestClass`,
            `{`,
            `    [Test]`,
            `    public void Test()`,
            `    {`,
            `        Console.Write("foobar");`,
            `        Assert.AreEqual(true, false);`,
            `    }`,
            `}`,
          ].join('\n')
        }, function(buffer) {
          expect(buffer.stdout).to.contain("\n<FAILED::>");
          done();
        });
      });
    });
  });
});
