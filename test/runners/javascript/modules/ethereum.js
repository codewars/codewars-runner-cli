"use strict";

const expect = require('chai').expect;

const runner = require('../../../runner');

describe('ethereum', function() {
  this.timeout(0);

  it('should support basic VM', function(done) {
    runner.run({
      language: 'javascript',
      code: `
        var VM = require('ethereumjs-vm');
        //create a new VM instance
        var vm = new VM()
        var code = '7f4e616d65526567000000000000000000000000000000000000000000000000003055307f4e616d6552656700000000000000000000000000000000000000000000000000557f436f6e666967000000000000000000000000000000000000000000000000000073661005d2720d855f1d9976f88bb10c1a3398c77f5573661005d2720d855f1d9976f88bb10c1a3398c77f7f436f6e6669670000000000000000000000000000000000000000000000000000553360455560df806100c56000396000f3007f726567697374657200000000000000000000000000000000000000000000000060003514156053576020355415603257005b335415603e5760003354555b6020353360006000a233602035556020353355005b60007f756e72656769737465720000000000000000000000000000000000000000000060003514156082575033545b1560995733335460006000a2600033545560003355005b60007f6b696c6c00000000000000000000000000000000000000000000000000000000600035141560cb575060455433145b1560d25733ff5b6000355460005260206000f3'
        
        vm.runCode({
          code: Buffer.from(code, 'hex'), // code needs to be a Buffer,
          gasLimit: Buffer.from('ffffffff', 'hex')
        }, function(err, results){
          console.log('returned: ' + results.return.toString('hex'));
        })
      `
    }, buffer => {
      expect(buffer.stdout.match(/^returned: .*/)).to.not.eq(null);
      done();
    });
  });

  it('should support merkle-patricia-tree', function(done) {
    runner.run({
      language: 'javascript',
      code: `
        var Trie = require('merkle-patricia-tree'),
        levelup = require('levelup'),
        db = levelup({ db: require('memdown') }),
        trie = new Trie(db); 
         
        trie.put('test', 'one', function () {
          trie.get('test', function (err, value) {
            if(value) console.log(value.toString())
          });
        });
      `
    }, buffer => {
      expect(buffer.stdout).to.contain('one');
      done();
    });
  });

  // https://raw.githubusercontent.com/ethereumjs/ethereumjs-vm/master/examples/run-blockchain/index.js
  it('should support blockchain', function(done) {
    runner.run({
      language: 'javascript',
      timeout: 60000, // for some reason this is really slow when ran with the entire suite
      code: `
        const Buffer = require('safe-buffer').Buffer // use for Node.js <4.5.0
        const async = require('async')
        const Trie = require('merkle-patricia-tree/secure')
        const Block = require('ethereumjs-block')
        const Blockchain = require('ethereumjs-blockchain')
        const BlockHeader = require('ethereumjs-block/header.js')
        const VM = require('ethereumjs-vm')
        const Level = require('levelup')
        const Account = require('ethereumjs-account')
        const utils = require('ethereumjs-util')
        const BN = utils.BN
        const rlp = utils.rlp
        const testData = require('/runner/test/runners/javascript/modules/ethereum/test-data')
        // inMemory blockchainDB
        var blockchainDB = new Level('', { db: require('memdown') })
        
        var state = new Trie()
        
        var blockchain = new Blockchain(blockchainDB)
        blockchain.ethash.cacheDB = new Level('./.cachedb')
        
        var vm = new VM({
          state: state,
          blockchain: blockchain
        })
        var genesisBlock = new Block()
        
        vm.on('beforeTx', function (tx) {
          tx._homestead = true
        })
        
        vm.on('beforeBlock', function (block) {
          block.header.isHomestead = function () {
            return true
          }
        })
        
        async.series([
          // set up pre-state
          function (next) {
            setupPreConditions(state, testData, next)
          },
        
          // create and add genesis block
          function (next) {
            genesisBlock.header = new BlockHeader(
                                    testData.genesisBlockHeader
                                  )
            blockchain.putGenesis(genesisBlock, next)
          },
        
          // add some following blocks
          function (next) {
            async.eachSeries(testData.blocks, eachBlock, next)
        
            function eachBlock (raw, cb) {
              try {
                var block = new Block(
                    Buffer.from(raw.rlp.slice(2), 'hex'))
        
                // forces the block into thinking they are homestead
                block.header.isHomestead = function () {
                  return true
                }
                block.uncleHeaders.forEach(function (uncle) {
                  uncle.isHomestead = function () { return true }
                })
        
                blockchain.putBlock(block, function (err) {
                  cb(err)
                })
              } catch (err) {
                cb()
              }
            }
          },
        
          // make sure the blocks check
          // if a block is missing, vm.runBlockchain will fail
          function runBlockchain (next) {
            vm.runBlockchain(next)
          },
        
          // get the blockchain head
          function getHead (next) {
            vm.blockchain.getHead(function (err, block) {
              // make sure the state is set before checking post conditions
              state.root = block.header.stateRoot
              next(err)
            })
          }
        ], function () {
          console.log('--- Finished processing the BlockChain ---')
          console.log('New head:', '0x' + blockchain.meta.rawHead.toString('hex'))
          console.log('Expected:', testData.lastblockhash)
        })
        
        function setupPreConditions (state, testData, done) {
          var keysOfPre = Object.keys(testData.pre)
        
          async.eachSeries(keysOfPre, function (key, callback) {
            var acctData = testData.pre[key]
            var account = new Account()
        
            account.nonce = format(acctData.nonce)
            account.balance = format(acctData.balance)
        
            var codeBuf = Buffer.from(acctData.code.slice(2), 'hex')
            var storageTrie = state.copy()
            storageTrie.root = null
        
            async.series([
              function (cb2) {
                var keys = Object.keys(acctData.storage)
        
                async.forEachSeries(keys, function (key, cb3) {
                  var val = acctData.storage[key]
                  val = rlp.encode(Buffer.from(val.slice(2), 'hex'))
                  key = utils.setLength(Buffer.from(key.slice(2), 'hex'), 32)
        
                  storageTrie.put(key, val, cb3)
                }, cb2)
              },
              function (cb2) {
                account.setCode(state, codeBuf, cb2)
              },
              function (cb2) {
                account.stateRoot = storageTrie.root
        
                if (testData.exec && key === testData.exec.address) {
                  testData.root = storageTrie.root
                }
                state.put(Buffer.from(key, 'hex'), account.serialize(), function () {
                  cb2()
                })
              }
            ], callback)
          }, done)
        }
        
        function format (a, toZero, isHex) {
          if (a === '') {
            return Buffer.alloc(0)
          }
        
          if (a.slice && a.slice(0, 2) === '0x') {
            a = a.slice(2)
            if (a.length % 2) a = '0' + a
            a = Buffer.from(a, 'hex')
          } else if (!isHex) {
            a = Buffer.from(new BN(a).toArray())
          } else {
            if (a.length % 2) a = '0' + a
            a = Buffer.from(a, 'hex')
          }
        
          if (toZero && a.toString('hex') === '') {
            a = Buffer.from([0])
          }
        
          return a
        }
      `
    }, buffer => {
      expect(buffer.stdout).to.include("--- Finished processing the BlockChain ---");
      done();
    });
  });

  it('should pre-compiled contracts', function(done) {
    runner.run({
      language: 'javascript',
      code: `
        var contract = require("truffle-contract");
        var TestRPC = require("ethereumjs-testrpc");
        var Web3 = require("web3");
        var provider = TestRPC.provider();
        var web3 = new Web3();
        web3.setProvider(provider)
        
        const Ballot = contract(require('/runner/frameworks/ethereum/contracts/Ballot.json'));
        Ballot.setProvider(provider);
      `
    }, buffer => {
      expect(buffer.stderr).to.equal('');
      done();
    });
  });

  it('web3 with testrpc', function(done) {
    runner.run({
      language: 'javascript',
      code: `
        var Web3 = require('web3');
        var web3 = new Web3();
        var TestRPC = require("ethereumjs-testrpc");
        web3.setProvider(TestRPC.provider());
        var account = web3.eth.accounts.create();
        console.log(account.address);
      `
    }, buffer => {
      expect(buffer.stdout).to.contain('0x');
      expect(buffer.stderr).to.equal('');
      done();
    });
  });
});
