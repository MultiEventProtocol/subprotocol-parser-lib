contract test_fn_state_mutability
{
    function fn_1 (uint p_one, int p_two) external view returns (uint r_one) ;
    function fn_2 (uint p_one, int p_two) internal payable returns (uint r_one) ;
    function fn_3 (uint p_one, int p_two) external view returns (uint r_one) ;
    function fn_4 (uint p_one, int p_two) view private returns (uint r_one) ;
}
