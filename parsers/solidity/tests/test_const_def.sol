contract test_const_def
{
    string public constant symbol = "TKN";
    bytes32 public constant hash = keccak256(symbol);
}
