contract test_funcdef_yul
{
    function fn_test_yul_for () {
        assembly {
        for { } lt(i, 0x100) { } {
           x := add(x, mload(i))
           i := add(i, 0x20)
          }
        }
    }
}
