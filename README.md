# subprotocol-parser-lib
library and demo of parsing MEP sub-protocol manifest

The MEP protocol manifest is a set of declarations that are needed to call the protocol API. For the convenience of web3 developers, it is written in Solidity. This library parses solidity code into an abstract syntax tree, which has a visual representation in the form of S-expressions. The resulting S-expressions can be easily parsed and converted into code that will do the main work. This code can be written in TypeScript, Rust, or Go, with S-expressions being an intermediate representation for all of these languages.

Example:

```solidity
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
```

Result:
```lisp
(:SRC
 ((:PRAGMA-DEF (:PRAGMA "solidity ^0.8.0;"))
  (:CTRACT-DEF
   (:CONTRACT "MEP_solidity_example_test" :CONTENTS
    (:CTRACT-BODY-ELT
     ((:STRUCT-DEF
       (:STRUCT "ContractId" :CONTENTS
        (:MEMBER
         ((:STRUCT-NAME "network" :TYPE (:ELT-TYPE-NAME (:STRING STRING)))
          (:STRUCT-NAME "contractAddr" :TYPE
           (:ELT-TYPE-NAME (:STRING STRING)))))))
      (:STRUCT-DEF
       (:STRUCT "ContractEvent" :CONTENTS
        (:MEMBER
         ((:STRUCT-NAME "contractId" :TYPE
           (:IDENTIFIER-PATH (:IDENT ((:ID "ContractId")))))
          (:STRUCT-NAME "EventId" :TYPE (:ELT-TYPE-NAME (:STRING STRING)))))))
      (:EVENT-DEF
       (:EVENT "SomeEvent" :PARAMS
        (:EVENT-PARAM
         ((:TYPE-NAME (:ELT-TYPE-NAME (:ADDRESS AD)) :ID "_from")
          (:TYPE-NAME (:ELT-TYPE-NAME (:UNSIGNED-INTEGER-TYPE UINT)) :ID
           "_value")))))
      (:FUNC-DEF
       (:FUN "SomeEventHandler" :PARLIST
        (:PARAM-LIST
         (:PAR
          ((:PAR-TYPE (:IDENTIFIER-PATH (:IDENT ((:ID "SomeContractEvent"))))
            :NAME "e"))))
        :FMETA (:VISIBILITY PUBLIC :STATE-MUTABILITY (:STATE-MUTABILITY VIEW))
        :RETLIST
        (:RETLIST
         (:PAR
          ((:PAR-TYPE (:IDENTIFIER-PATH (:IDENT ((:ID "SomeMEPEvent"))))))))
        :BLOCK
        (:STMNT
         (:VAR-DECL-STMNT
          ((:VAR-DECL
            (:VAR-TYPE (:IDENTIFIER-PATH (:IDENT ((:ID "SomeMEPEvent")))) :NAME
             "mep_e")
            :INIT (:EXPR-TN (:IDENTIFIER-PATH (:IDENT ((:ID "e"))))))
           (:RETURN
            (:EXPR-TN (:IDENTIFIER-PATH (:IDENT ((:ID "e")))))))))))))))))
```

## install and prepare to work
```sh
git clone git@github.com:MultiEventProtocol/subprotocol-parser-lib.git
cd subprotocol-parser-lib/
make build
make demo
```
