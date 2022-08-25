contract test_try_stmnt
{
    function fn_try_stmnt (uint p_one) returns (uint r_one) {
        try 123 { } catch { }
        try 123 { } catch { } catch { }
        try 123 { } catch (uint memory reason) { }
        try 123 { } catch Error(uint memory reason) { }
        try 123 returns (int memory retval)
            { } catch Error(uint memory reason) { }
    }
}
