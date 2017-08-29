pragma solidity ^0.4.13;

import "./setup.sol";

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

  function getBalanceInEth(address addr) returns(uint) {
    return ConvertLib.convert(getBalance(addr), 2);
  }

  function getBalance(address addr) returns(uint) {
    return balances[addr];
  }

  // for time travel demo, only valid for a day
  function isValid() returns(bool) {
    return now <= endTime;
  }
}
