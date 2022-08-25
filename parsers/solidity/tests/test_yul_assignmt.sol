contract test_assignment
{
    function fn_test_yul_assignment () {
        assembly {
            smth := ""
            smth, obj.IsOk := some_call()
            smth := "abc"
            smth := 1234
            smth := obj.IsOk
            smth := some_call(obj.IsOk)

               }
    }
}
