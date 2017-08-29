pragma solidity ^0.4.13;

library ConvertLib {
  function convert(uint amount, uint conversionRate) returns (uint convertedAmount) {
    return amount * conversionRate;
  }
}
