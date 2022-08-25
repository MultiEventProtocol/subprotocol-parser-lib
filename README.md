# subprotocol-parser-lib
library and demo of parsing MEP sub-protocol manifest

The MEP protocol manifest is a set of declarations that are needed to call the protocol API. For the convenience of web3 developers, it is written in Solidity.

This library parses solidity code into an abstract syntax tree, which has a visual representation in the form of S-expressions. The resulting S-expressions can be easily transformed into code (in TypeScript, Rust, or Go) or compiled into bytecode for virtual machine, which we will write.

Below we show all these steps:
- Parsing a simplified example of solidity code
- obtaining an S-expression tree
- transforming the tree and generating the resulting Rust code

We plan (as part of this project) in the near future to develop a virtual machine as a target backend for compiling from S-expressions to bytecode. See section "Virtual environment for execution".

## how it can parse solidity into s-expressions

Example of solidity code (mep.sol):

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

To turn solidity code into an s-expression tree:
```bash
./solparser -p ./mep.sol -o mep.sexp
```

"-p" means "parse", "-o" means "output"

Result (file mep.sexp):
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

## how we can turn the lisp into the rust library

```bash
./transpiler -t mep.sexp -o mep.rs
```

"-t" means "transpile", "-o" means "output"

Result (file mep.rs) - work in progress:
```rust
struct ContractId {
    network: String,
    contract_addr: String,
}

struct ContractEvent {
    contract_id: ContractId,
    event_id: String,
}

// event
fn some_event(_from: u32, _value: u32) {
}

fn some_event_handler(e: SomeContractEvent) -> SomeMEPEVent {
    let mep_e = e;
    return e;
}

fn main() {
    println!("Hi!");
}

```

## virtual environment for execution

Smart contracts can be vulnerable to hacker attacks, so running them inside general-purpose programming languages is dangerous. In the case of rast, the module can be compiled in WASM and executed in a sandbox.

But a more secure method would be to execute the bytecode in a specially created virtual machine, as is done in Etherium.

We plan to develop a set of opcodes and a reference implementation of such a virtual machine during this year to provide a secure and reliable environment for execution.

## build project
```sh
git clone git@github.com:MultiEventProtocol/subprotocol-parser-lib.git
cd subprotocol-parser-lib/
sudo apt install emacs
make all
```
