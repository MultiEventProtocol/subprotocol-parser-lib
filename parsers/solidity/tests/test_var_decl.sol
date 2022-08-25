contract test_var_decl
{
    function fn_var_decl (uint p_one) returns (uint r_one) {
        uint first_var;
        uint memory second_var;
        int init_var_third = 123;
        int memory init_var_fourth = 234;
    }
}
