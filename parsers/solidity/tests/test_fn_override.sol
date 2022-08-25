contract test_fn_override
{
    function fn_1 (uint p_one) override returns (uint r_one) ;
    function fn_1 (uint p_one) override(Base1, Obj.Base2) returns (uint r_one) ;
}
