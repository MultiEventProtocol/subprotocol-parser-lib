pragma solidity ^0.8.0;

import "../proxy/Clones.sol";

contract CompTimelock {

    /* constructor( */
    /*             string memory name, */
    /*             string memory symbol, */
    /*             uint256 cap */
    /* ) ERC20(name, symbol) ERC20Capped(cap) {} */

    /* constructor( */
    /*     string memory name, */
    /*     string memory symbol, */
    /*     address initialAccount, */
    /*     uint256 initialBalance */
    /* ) payable ERC20(name, symbol) ERC20Permit(name) { */
    /*     _mint(initialAccount, initialBalance); */
    /* } */

    uint256 public constant GRACE_PERIOD = 14 days;

    function test1(History storage self, uint256 blockNumber) internal view returns (uint256) {

        require(blockNumber < block.number, "Checkpoints: block not yet mined");
        Checkpoint({_blockNumber: SafeCast.toUint32(block.number), _value: SafeCast.toUint224(value)});
        return high == 0 ? 0 : self._checkpoints[high - 1]._value;
    }

    function test(History storage self, uint256 blockNumber) internal view returns (uint256) {
        assembly {
            let ptr := mload(0x4)
                mstore(ptr, 0x0000000000000000000000000000000011111142000000)

                sender := shr(96, calldataload(sub(calldatasize(), 20)))

                }
        return 0x0000000000000000000000000000000011111142;
    }

    function throwError(RecoverError error) private pure {
        if (error == RecoverError.NoError) {
            return; // no error: do nothing
        }    else if (error == RecoverError.InvalidSignature) {
            revert("ECDSA: invalid signature");
        }    else if (error == RecoverError.InvalidSignatureLength) {
            revert("ECDSA: invalid signature length");
        }    else if (error == RecoverError.InvalidSignatureS) {
            revert("ECDSA: invalid signature 's' value");
        }    else if (error == RecoverError.InvalidSignatureV) {
            revert("ECDSA: invalid signature 'v' value");
        }
    }


    function increment(Counter storage counter) internal {

        (bool success, bytes memory returndata) = address(proxy).staticcall(hex"dhbj");

        (address recovered, ECDSA.RecoverError error) = ECDSA.tryRecover(hash, signature);

        require(blockNumber < block.number, "Checkpoints: block not yet mined");

        unchecked {
            counter._value += 1;
        }
        return msg.data[:msg.data.length - 20];
    }
}
