contract test_fn_returns
{
    function fn_1 (uint storage p_one) returns (uint storage r_one, int memory r_two) ;
    function fn_2 (uint storage p_one, int memory p_two) returns () ;
    function fn_3 (uint p_one, int p_two) returns (uint r_one) ;
}
