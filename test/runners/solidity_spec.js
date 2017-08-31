"use strict";

const exec = require('child_process').exec;

const expect = require('chai').expect;

const runner = require('../runner');

describe('truffle test', function() {
  afterEach(function cleanup(done) {
    exec('rm -rf /workspace/solidity/contracts/solution.sol /workspace/solidity/contracts/setup.sol /tmp/test-*', err => err ? done(err) : done());
  });

  it('should handle basic assertion', function(done) {
    this.timeout(0);
    runner.run({
      language: 'solidity',
      solution: `
        pragma solidity ^0.4.13;

        contract MetaCoin {
          mapping (address => uint) balances;
          function MetaCoin() {
            balances[tx.origin] = 10000;
          }
          function getBalance(address addr) returns(uint) {
            return balances[addr];
          }
        }
      `,
      fixture: `
        const MetaCoin = artifacts.require("MetaCoin");
        contract('MetaCoin', function(accounts) {
          it("should put 10000 MetaCoin in the first account", async function() {
            const m = await MetaCoin.deployed();
            const balance = await m.getBalance.call(accounts[0]);
            assert.equal(balance.valueOf(), 10000, "10000 wasn't in the first account");
          });
        });
      `,
    }, function(buffer) {
      expect(buffer.stdout).to.contain('\n<PASSED::>');
      expect(buffer.stdout).not.to.contain('\n<FAILED::>');
      expect(buffer.stderr).to.be.empty;
      done();
    });
  });

  it('should handle basic assertion failure', function(done) {
    this.timeout(0);
    runner.run({
      language: 'solidity',
      solution: `
        pragma solidity ^0.4.13;

        contract MetaCoin {
          mapping (address => uint) balances;
          function MetaCoin() {
            balances[tx.origin] = 10001;
          }
          function getBalance(address addr) returns(uint) {
            return balances[addr];
          }
        }
      `,
      fixture: `
        const MetaCoin = artifacts.require("MetaCoin");
        contract('MetaCoin', function(accounts) {
          it("should put 10000 MetaCoin in the first account", async function() {
            const m = await MetaCoin.deployed();
            const balance = await m.getBalance.call(accounts[0]);
            assert.equal(balance.valueOf(), 10000, "10000 wasn't in the first account");
          });
        });
      `,
    }, function(buffer) {
      expect(buffer.stdout).to.contain('\n<FAILED::>');
      done();
    });
  });

  it('should handle basic assertion failure from error', function(done) {
    this.timeout(0);
    runner.run({
      language: 'solidity',
      solution: `
        pragma solidity ^0.4.13;

        contract MetaCoin {
          mapping (address => uint) balances;
          function MetaCoin() {
            balances[tx.origin] = 10001;
          }
          function getBalance(address addr) returns(uint) {
            return balances[addr];
          }
        }
      `,
      fixture: `
        const MetaCoin = artifacts.require("MetaCoin");
        contract('MetaCoin', function(accounts) {
          it("should put 10000 MetaCoin in the first account", async function() {
            const m = await MetaCoin.deployed();
            const balance = await m.getBalance.call(accounts[0]);
            await m.foo.call();
            assert.equal(balance.valueOf(), 10000, "10000 wasn't in the first account");
          });
        });
      `,
    }, function(buffer) {
      expect(buffer.stdout).to.contain('\n<ERROR::>');
      done();
    });
  });

  it('should output correct format', function(done) {
    this.timeout(0);
    runner.run({
      language: 'solidity',
      solution: `
        pragma solidity ^0.4.13;

        contract MetaCoin {
          mapping (address => uint) balances;
          function MetaCoin() {
            balances[tx.origin] = 10000;
          }
          function getBalance(address addr) returns(uint) {
            return balances[addr];
          }
        }
      `,
      fixture: `
        const MetaCoin = artifacts.require("MetaCoin");
        contract('MetaCoin', function(accounts) {
          it("should put 10000 MetaCoin in the first account", async function() {
            const m = await MetaCoin.deployed();
            const balance = await m.getBalance.call(accounts[0]);
            assert.equal(balance.valueOf(), 10000, "10000 wasn't in the first account");
          });
        });
      `,
    }, function(buffer) {
      const expected = [
        '<DESCRIBE::>',
        '  <IT::><PASSED::><COMPLETEDIN::>',
        '<COMPLETEDIN::>',
      ].join('').replace(/\s/g, '');
      expect(buffer.stdout.match(/<(?:DESCRIBE|IT|PASSED|FAILED|COMPLETEDIN)::>/g).join('')).to.equal(expected);
      expect(buffer.stderr).to.be.empty;
      done();
    });
  });

  it('should output nested describes', function(done) {
    this.timeout(0);
    runner.run({
      language: 'solidity',
      solution: `
        pragma solidity ^0.4.13;

        contract MetaCoin {
          mapping (address => uint) balances;
          function MetaCoin() {
            balances[tx.origin] = 10000;
          }
          function getBalance(address addr) returns(uint) {
            return balances[addr];
          }
        }
      `,
      fixture: `
        const MetaCoin = artifacts.require("MetaCoin");
        describe("contracts", function() {
          contract('MetaCoin', function(accounts) {
            it("should put 10000 MetaCoin in the first account", async function() {
              const m = await MetaCoin.deployed();
              const balance = await m.getBalance.call(accounts[0]);
              assert.equal(balance.valueOf(), 10000, "10000 wasn't in the first account");
            });
          });
        });
      `,
    }, function(buffer) {
      const expected = [
        '<DESCRIBE::>',
        '  <DESCRIBE::>',
        '    <IT::><PASSED::><COMPLETEDIN::>',
        '  <COMPLETEDIN::>',
        '<COMPLETEDIN::>',
      ].join('').replace(/\s/g, '');
      expect(buffer.stdout.match(/<(?:DESCRIBE|IT|PASSED|FAILED|COMPLETEDIN)::>/g).join('')).to.equal(expected);
      expect(buffer.stderr).to.be.empty;
      done();
    });
  });


  it('should support opts.setup', function(done) {
    this.timeout(0);
    runner.run({
      language: 'solidity',
      setup: `
        pragma solidity ^0.4.13;

        library ConvertLib {
          function convert(uint amount, uint conversionRate) returns (uint convertedAmount) {
            return amount * conversionRate;
          }
        }
      `,
      solution: `
        pragma solidity ^0.4.13;

        import "./setup.sol";

        contract MetaCoin {
          mapping (address => uint) balances;

          function MetaCoin() {
            balances[tx.origin] = 10000;
          }
          function getBalanceInEth(address addr) returns(uint) {
            return ConvertLib.convert(getBalance(addr), 2);
          }
          function getBalance(address addr) returns(uint) {
            return balances[addr];
          }
        }
      `,
      fixture: `
        const MetaCoin = artifacts.require("MetaCoin");
        contract('MetaCoin', function(accounts) {
          it("should call a function that depends on a linked library", async function() {
            const m = await MetaCoin.deployed();
            const coinBalance = await m.getBalance.call(accounts[0]);
            const coinEthBalance = await m.getBalanceInEth.call(accounts[0]);
            assert.equal(coinEthBalance.toNumber(), 2*coinBalance.toNumber(), "Library function returned unexpected function, linkage may be broken");
          });
        });
      `,
    }, function(buffer) {
      expect(buffer.stdout).to.contain('\n<PASSED::>');
      expect(buffer.stdout).not.to.contain('\n<FAILED::>');
      done();
    });
  });

  it('should handle time traveling', function(done) {
    this.timeout(0);
    runner.run({
      language: 'solidity',
      solution: `
        pragma solidity ^0.4.13;

        contract MetaCoin {
          uint public endTime; // for time-travel demo

          function MetaCoin() {
            endTime = now + 1 days;
          }
          function isValid() returns(bool) { // for time travel demo, only valid for a day
            return now <= endTime;
          }
        }
      `,
      fixture: `
        const MetaCoin = artifacts.require("MetaCoin");

        const Web3 = require('web3');
        const web3 = new Web3();
        web3.setProvider(new Web3.providers.HttpProvider("http://localhost:8545"));

        contract('MetaCoin', function(accounts) {
          it("should be valid if the time is within a day from creation", async function() {
            const m = await MetaCoin.new();
            await increaseTime(10); // 10 seconds
            assert.equal(await m.isValid.call(), true, "status wasn't valid after 10 seconds");
          });

          it("should be invalid after a day", async function() {
            const m = await MetaCoin.new();
            await increaseTime(2 * 86400); // 2 days later
            assert.equal(await m.isValid.call(), false, "status was valid even after a day");
          });
        });

        function increaseTime(seconds) {
          return new Promise((resolve, reject) => {
            web3.currentProvider.sendAsync({
              jsonrpc: "2.0",
              method: "evm_increaseTime",
              params: [seconds],
              id: new Date().getTime()
            }, (err, result) => {
              if (err) return reject(err);
              // resolve(result);
              // HACK workarounds https://github.com/ethereumjs/testrpc/issues/336
              mineBlock().then(_ => resolve(result)).catch(reject);
            });
          });
        }

        function mineBlock() {
          return new Promise((resolve, reject) => {
            web3.currentProvider.sendAsync({
              jsonrpc: "2.0",
              method: "evm_mine"
            }, (err, result) => err ? reject(err) : resolve(result));
          });
        }
      `,
    }, function(buffer) {
      expect(buffer.stdout).to.contain('\n<PASSED::>');
      expect(buffer.stdout).not.to.contain('\n<FAILED::>');
      done();
    });
  });

  it('tests can access and output some information', function(done) {
    this.timeout(0);
    runner.run({
      language: 'solidity',
      solution: `
        pragma solidity ^0.4.13;

        contract MetaCoin {
          mapping (address => uint) balances;

          event Transfer(address indexed _from, address indexed _to, uint256 _value);

          uint public endTime; // for time-travel demo

          function MetaCoin() {
            balances[tx.origin] = 10000;
            endTime = now + 1 days;
          }
          function sendCoin(address receiver, uint amount) returns(bool sufficient) {
            if (balances[msg.sender] < amount) return false;
            balances[msg.sender] -= amount;
            balances[receiver] += amount;
            Transfer(msg.sender, receiver, amount);
            return true;
          }
          function getBalance(address addr) returns(uint) {
            return balances[addr];
          }
          function isValid() returns(bool) { // for time travel demo, only valid for a day
            return now <= endTime;
          }
        }
      `,
      fixture: `
        const MetaCoin = artifacts.require("MetaCoin");

        const Web3 = require('web3');
        const web3 = new Web3();
        web3.setProvider(new Web3.providers.HttpProvider("http://localhost:8545"));

        contract('MetaCoin', function(accounts) {
          before(async () => {
            console.log('<LOG:JSON:-Available Accounts>' + JSON.stringify(accounts));
          });

          it("should put 10000 MetaCoin in the first account", async function() {
            const m = await MetaCoin.deployed();
            const balance = await m.getBalance.call(accounts[0]);
            console.log('<LOG:JSON:-Block>' + JSON.stringify(web3.eth.getBlock(web3.eth.blockNumber)));
            console.log('<LOG::Current Gas Price>' + web3.eth.gasPrice.toString(10));
            assert.equal(balance.valueOf(), 10000, "10000 wasn't in the first account");
          });

          it("should send coin correctly", async function() {
            const amount = 10;
            const m = await MetaCoin.deployed();
            // Get initial balances of first and second account.
            const account1 = accounts[0], account2 = accounts[1];
            const account1Start = (await m.getBalance.call(account1)).toNumber();
            const account2Start = (await m.getBalance.call(account2)).toNumber();

            const result = await m.sendCoin(account2, amount, {from: account1});
            const receipt = result.receipt;
            console.log('<LOG:JSON:-Transaction Receipt>' + JSON.stringify({
              transactionHash: receipt.transactionHash,
              blockNumber: receipt.blockNumber,
              gasUsed: receipt.gasUsed,
              cumulativeGasUsed: receipt.cumulativeGasUsed,
            }));
            for (const log of result.logs) {
              console.log('<LOG:JSON:-Transaction Log ' + log.logIndex + '>' + JSON.stringify(log));
            }

            const account1End = (await m.getBalance.call(account1)).toNumber();
            const account2End = (await m.getBalance.call(account2)).toNumber();

            assert.equal(account1End, account1Start - amount, "Amount wasn't correctly taken from the sender");
            assert.equal(account2End, account2Start + amount, "Amount wasn't correctly sent to the receiver");
          });
        });
      `,
    }, function(buffer) {
      expect(buffer.stdout).to.contain('<LOG:JSON:-Available Accounts>');
      expect(buffer.stdout).to.contain('<LOG:JSON:-Block>');
      expect(buffer.stdout).to.contain('<LOG:JSON:-Transaction Receipt>');
      expect(buffer.stdout).to.contain('<LOG:JSON:-Transaction Log');
      done();
    });
  });

  it('should support library in same file', function(done) {
    this.timeout(0);
    runner.run({
      language: 'solidity',
      solution: `
        pragma solidity ^0.4.13;

        library Foo {
          function add(uint x, uint y) returns(uint) {
            return x + y;
          }
        }
        contract Bar {
          function sum(uint x, uint y) returns(uint) {
            return Foo.add(x, y);
          }
        }
      `,
      fixture: `
        const Bar = artifacts.require("Bar");
        contract('Bar', function(accounts) {
          it("should return the sum", async function() {
            const m = await Bar.deployed();
            const v = await m.sum.call(1, 1);
            assert.equal(v.valueOf(), 2);
          });
        });
      `,
    }, function(buffer) {
      expect(buffer.stdout).to.contain('\n<PASSED::>');
      expect(buffer.stdout).not.to.contain('\n<FAILED::>');
      expect(buffer.stderr).to.be.empty;
      done();
    });
  });

  it('should support multiple libraries in same file', function(done) {
    this.timeout(0);
    runner.run({
      language: 'solidity',
      solution: `
        pragma solidity ^0.4.13;

        library Foo {
          function fun(uint x, uint y) returns(uint) {
            return x + y;
          }
        }

        library Bar {
          function fun(uint x, uint y) returns(uint) {
            return x - y;
          }
        }

        contract Buz {
          function fun(uint x, uint y) returns(uint) {
            return Bar.fun(Foo.fun(x, y), y);
          }
        }
      `,
      fixture: `
        const Buz = artifacts.require("Buz");
        contract('Buz', function(accounts) {
          it("should return x: (x + y) - y", async function() {
            const m = await Buz.deployed();
            const v = await m.fun.call(1, 1);
            assert.equal(v.valueOf(), 1);
          });
        });
      `,
    }, function(buffer) {
      expect(buffer.stdout).to.contain('\n<PASSED::>');
      expect(buffer.stdout).not.to.contain('\n<FAILED::>');
      expect(buffer.stderr).to.be.empty;
      done();
    });
  });

  it('should support multiple libraries setup', function(done) {
    this.timeout(0);
    runner.run({
      language: 'solidity',
      setup: `
        pragma solidity ^0.4.13;

        library Foo {
          function fun(uint x, uint y) returns(uint) {
            return x + y;
          }
        }

        library Bar {
          function fun(uint x, uint y) returns(uint) {
            return x - y;
          }
        }
      `,
      solution: `
        pragma solidity ^0.4.13;

        import "./setup.sol";

        contract Buz {
          function fun(uint x, uint y) returns(uint) {
            return Bar.fun(Foo.fun(x, y), y);
          }
        }
      `,
      fixture: `
        const Buz = artifacts.require("Buz");
        contract('Buz', function(accounts) {
          it("should return x: (x + y) - y", async function() {
            const m = await Buz.deployed();
            const v = await m.fun.call(1, 1);
            assert.equal(v.valueOf(), 1);
          });
        });
      `,
    }, function(buffer) {
      expect(buffer.stdout).to.contain('\n<PASSED::>');
      expect(buffer.stdout).not.to.contain('\n<FAILED::>');
      expect(buffer.stderr).to.be.empty;
      done();
    });
  });

  it('should support zeppelin', function(done) {
    this.timeout(0);
    runner.run({
      language: 'solidity',
      solution: `
        pragma solidity ^0.4.4;
        import "zeppelin-solidity/contracts/token/StandardToken.sol";
        
        contract ExampleToken is StandardToken {
          string public name = "ExampleToken"; 
          string public symbol = "EGT";
          uint public decimals = 18;
          uint public INITIAL_SUPPLY = 10000;
        
          function ExampleToken() {
            totalSupply = INITIAL_SUPPLY;
            balances[msg.sender] = INITIAL_SUPPLY;
          }
        }
      `,
      fixture: `
        const Example = artifacts.require("ExampleToken");
        contract('Example', function(accounts) {
          it("should put 10000 in the first account", async function() {
            const m = await Example.deployed();
            
            const balance = await m.balanceOf.call(accounts[0]);
            assert.equal(balance.valueOf(), 10000, "10000 wasn't in the first account");
          });
        });
      `,
    }, function(buffer) {
      expect(buffer.stdout).to.contain('\n<PASSED::>');
      expect(buffer.stdout).not.to.contain('\n<FAILED::>');
      expect(buffer.stderr).to.be.empty;
      done();
    });
  });
});
