contract test_fn_virtual
{
    function fn_1 (uint p_one, int p_two) virtual returns (uint r_one) ;
    function fn_2 (uint p_one, int p_two) internal virtual returns (uint r_one) ;
    function fn_3 (uint p_one, int p_two) virtual external view returns (uint r_one) ;
    function fn_4 (uint p_one, int p_two) view private virtual returns (uint r_one) ;
}
