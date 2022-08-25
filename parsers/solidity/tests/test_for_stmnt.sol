contract test_for_stmnt
{
    function fn_for_stmnt (uint p_one) returns (uint r_one) {
        for (            ;     ; ) 345 ;
        for (            ;     ; ) { 345 ; }
        for ( uint idx   ;     ; ) { 345 ; }
        for ( uint idx=0 ;     ; ) { 345 ; }
        for ( 123        ;     ; ) { 345 ; }

        for (            ; 234 ; ) { 345 ; }
        for ( uint idx=0 ; 234 ; ) { 345 ; }
        for ( 123        ; 234 ; ) { 345 ; }

        for (            ;     ; 987 ) { 345 ; }
        for ( uint idx=0 ;     ; 987 ) { 345 ; }
        for ( 123        ;     ; 987 ) { 345 ; }

        for (            ; 234 ; 987 ) { 345 ; }
        for ( uint idx=0 ; 234 ; 987 ) { 345 ; }
        for ( 123        ; 234 ; 987 ) { 345 ; }
    }
}
