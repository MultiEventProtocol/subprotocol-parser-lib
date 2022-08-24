pragma solidity ^0.8.0;

contract MEP_solidity_example_test {

    struct ContractId {
        string network;
        string contractAddr;
    }

    struct ContractEvent {
        ContractId contractId;
        string EventId;
    }

    event SomeEvent(address _from, uint _value);

    function SomeEventHandler(SomeContractEvent e)
         public view returns(SomeMEPEvent){
         SomeMEPEvent mep_e = e;
         return e;
    }

}
