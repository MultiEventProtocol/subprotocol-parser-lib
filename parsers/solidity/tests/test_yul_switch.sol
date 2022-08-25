contract test_funcdef_yul
{
    function fn_test_yul_funcdef () {
        assembly {
            function some_function () -> smth1, smth2 {
                switch calldataload(4)
                       case 0 {
                       x := calldataload(0x24)
                       }
                       default {
                       x := calldataload(0x44)
                      }
            }
        }
    }
}
