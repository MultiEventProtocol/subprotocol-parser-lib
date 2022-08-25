contract test_empty_yul_block
{
    function fn_yul_dec_lit (uint p_one) returns (uint r_one) {
        1924;
        assembly {
                   let smth := 1924740
                   let smth := 001924740
                 }
    }
}
