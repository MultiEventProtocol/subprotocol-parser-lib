contract test_funcdef_yul
{
    function fn_test_yul_funcdef () {
        assembly {
            function some_function () -> smth1, smth2 {
            if 123 let smth := 1234
           }
        }
    }
}
