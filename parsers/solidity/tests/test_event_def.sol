contract test_event_def
{
    event some_1();
    event some_2() anonymous;

    event some_3(bytes);
    event some_4(bytes) anonymous;

    event some_5(uint, int);
    event some_6(uint, int) anonymous;

    event Deposit_7(address indexed from, bytes32 indexed id, uint val);
    event Deposit_8(address indexed from, bytes32 indexed id, uint val) anonymous;
}
