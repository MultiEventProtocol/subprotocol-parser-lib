contract test_mapping_type
{
    function fn_mapping_type_1 (uint8 p_one) returns (uint16 r_one) {
       new mapping(uint8 => uint16);
       new mapping(Obj.isOk => uint16);
    }
}
