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