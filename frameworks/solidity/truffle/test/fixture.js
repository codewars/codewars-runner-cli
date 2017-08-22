// Example tests produced by `truffle init`, changed to use async/await and added time-travel demo
const MetaCoin = artifacts.require("MetaCoin");

const Web3 = require('web3');
const web3 = new Web3();
web3.setProvider(new Web3.providers.HttpProvider("http://localhost:8545"));

contract('MetaCoin', function(accounts) {
  it("should put 10000 MetaCoin in the first account", async function() {
    const m = await MetaCoin.deployed();
    const balance = await m.getBalance.call(accounts[0]);
    assert.equal(balance.valueOf(), 10000, "10000 wasn't in the first account");
  });

  it("should call a function that depends on a linked library", async function() {
    const m = await MetaCoin.deployed();
    const coinBalance = await m.getBalance.call(accounts[0]);
    const coinEthBalance = await m.getBalanceInEth.call(accounts[0]);
    assert.equal(coinEthBalance.toNumber(), 2*coinBalance.toNumber(), "Library function returned unexpected function, linkage may be broken");
  });

  it("should send coin correctly", async function() {
    const amount = 10;
    const m = await MetaCoin.deployed();
    // Get initial balances of first and second account.
    const account1 = accounts[0], account2 = accounts[1];
    const account1Start = (await m.getBalance.call(account1)).toNumber();
    const account2Start = (await m.getBalance.call(account2)).toNumber();

    await m.sendCoin(account2, amount, {from: account1});

    const account1End = (await m.getBalance.call(account1)).toNumber();
    const account2End = (await m.getBalance.call(account2)).toNumber();

    assert.equal(account1End, account1Start - amount, "Amount wasn't correctly taken from the sender");
    assert.equal(account2End, account2Start + amount, "Amount wasn't correctly sent to the receiver");
  });

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
