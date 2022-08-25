contract test_empty_yul_block
{
    function fn_yul_var_decl (uint p_one) returns (uint r_one) {
        assembly {
                    let some
                    let some := obj.IsOk
                    let size := extcodesize(_addr)
                    let size := some(obj.IsOk)
                 }
    }
}
