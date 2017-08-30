const solc = require('solc'),
      fs = require('fs'),
      Artifactor = new require("truffle-artifactor"),
      contract = require("truffle-contract");

//var contractName = 'Greeter';
if (process.argv.length < 3) {
  console.log('needs the contract Name as argument!');
  process.exit(1);
}

const contractName = process.argv[2],
      source = fs.readFileSync(`/runner/frameworks/ethereum/contracts/${contractName}.sol`, {encoding: 'utf8'}),
      compiled = solc.compile(source, 1),
      contractKey = `:${contractName}`;

if (!compiled.contracts[contractKey]) {
  console.log('Contract must have same name as file!', Object.keys(compiled.contracts));
  process.exit(1);
}

const compiledContract = compiled.contracts[contractKey],
      contract_data = {
        contract_name: contractName,
        abi: JSON.parse(compiledContract.interface),
        binary: compiledContract.bytecode
      },
      outputDir = `/runner/frameworks/ethereum/contracts/`,
      artifactor = new Artifactor(outputDir);

artifactor.save(contract_data).then(() => {
  const file = `${outputDir}/${contractName}.json`;
  // load the contract just to make sure nothing fails
  contract(require(file));
  console.log(`File ${file} was created with the JS contract!`);
});
