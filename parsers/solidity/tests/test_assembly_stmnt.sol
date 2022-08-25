contract test_assembly_stmnt
{
    function fn_assembly_stmnt (uint p_one) returns (uint r_one) {
        assembly "evmasm" { }
    }
}
