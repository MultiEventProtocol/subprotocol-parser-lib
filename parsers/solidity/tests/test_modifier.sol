contract test_modifier
{
    modifier onlyOwner ;
    modifier onlyOwner () ;
    modifier onlyOwner () { }
    modifier onlyOwner () virtual { }

    modifier onlyOwner() {
        require(owner() == _msgSender(), "Ownable: caller is not the owner");
        _;
    }
}
