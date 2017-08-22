var ConvertLib = artifacts.require("ConvertLib");
var MetaCoin = artifacts.require("MetaCoin");

module.exports = function(deployer) {
  deployer.deploy(ConvertLib);
  deployer.link(ConvertLib, [MetaCoin]);
  deployer.deploy(MetaCoin);
};
