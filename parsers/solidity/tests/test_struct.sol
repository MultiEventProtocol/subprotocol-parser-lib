struct MapEntryPrimo {
    bytes32 _key;
    mapping (bytes32 => uint256) _indexes;
}

contract test_struct
{
    struct MapEntry {
        bytes32 _key;
        mapping (bytes32 => uint256) _indexes;
    }
}
