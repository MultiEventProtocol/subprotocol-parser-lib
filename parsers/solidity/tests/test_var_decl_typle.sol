contract test_var_decl_tuple
{
    function fn_var_decl_typle (uint p_one) returns (uint r_one) {
        (int one) = 1;
        (int memory two) = 2;
        (int ab, uint cd) = 3;
        (int ab,) = 4;
        (int ab, uint cd, int ef) = 5;
        (int ab, , ,) = 6;
        (int gh, , uint hi, , uint jk ,) = 7;
    }
}
