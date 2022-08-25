contract test_revert_stmnt
{
    function fn_revert_stmnt (uint p_one) returns (uint r_one) {
        revert 123 ( ) ;
        revert 123 ( 321 ) ;
        revert 123 ( 321, 432 ) ;
        revert 123 ( { } ) ;
        revert 123 ( { Log : 987 } ) ;
        revert 123 ( { Log : 987, Some : 765 } ) ;
        revert(someError);
    }
}
