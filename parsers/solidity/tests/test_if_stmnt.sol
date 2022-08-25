contract test_if_stmnt
{
    function fn_if_stmnt (uint p_one) returns (uint r_one) {
        if ( 123 ) 345 ;
        if ( 123 ) { 345; }
        if ( 123 ) 345 ; else 998 ;
        if ( 123 ) { 345 ; } else 998 ;
        if ( 123 ) { 345 ; } else { 998 ; }
    }
}
