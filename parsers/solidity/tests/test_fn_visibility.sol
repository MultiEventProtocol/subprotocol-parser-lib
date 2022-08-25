contract test_fn_visibility
{
    function fn_1 (uint p_one, int p_two) external returns (uint r_one) ;
    function fn_2 (uint p_one, int p_two) internal returns (uint r_one) ;
    function fn_2 (uint p_one, int p_two) internal ;
}
