contract test_emit_stmnt
{
    function fn_emit_stmnt (uint p_one) returns (uint r_one) {
        emit 123 ( ) ;
        emit 123 ( 321 ) ;
        emit 123 ( 321, 432 ) ;
        emit 123 ( { } ) ;
        emit 123 ( { Log : 987 } ) ;
        emit 123 ( { Log : 987, Some : 765 } ) ;
    }
}
