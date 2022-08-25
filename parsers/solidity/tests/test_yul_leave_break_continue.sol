contract test_empty_yul_block
{
      function fn_yul_empty_stmnt (uint p_one) returns (uint r_one) {

          assembly { continue break leave }
      }
}
