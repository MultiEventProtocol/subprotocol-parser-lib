contract test_fn_unchecked
{
    function fn_unchecked_1 () ;
    function fn_unchecked_2 () {
        unchecked { }
    }
    function fn_unchecked_3 () {
        unchecked { }
        unchecked { }
    }
    function fn_unchecked_3 () {
        unchecked { unchecked { } }
    }
}
