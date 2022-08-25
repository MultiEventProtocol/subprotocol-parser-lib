contract test_if_yul
{
    function fn_test_yul_if () {
        if ( 123 ) 345 ;

        assembly {
            if 123 let smth := 1234
            if ""  smth := some_call()
            if "absgd"  smth := some_call(obj.done)
            if obj.IsOk  {}
        }
    }
}
