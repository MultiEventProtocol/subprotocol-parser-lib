contract test_yul_bool_lit
{
    function fn_bool_lit (uint p_one) returns (uint r_one) {
        true;
        assembly {
            let smth := true
                smth2(false)
                }

    }
}
