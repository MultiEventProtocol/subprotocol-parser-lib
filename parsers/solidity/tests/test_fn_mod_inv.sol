contract test_fn_mod_inv
{
    function fn_1 (uint p_one) isOk returns (uint r_one) ;
    function fn_1 (uint p_one) Obj.isOk returns (uint r_one) ;
    function fn_1 (uint p_one) isOk() returns (uint r_one) ;
    function fn_2 (uint p_one) Obj.isOk() returns (uint r_one) ;
    function fn_3 (uint p_one) One.Two.isOk() returns (uint r_one) ;
}
