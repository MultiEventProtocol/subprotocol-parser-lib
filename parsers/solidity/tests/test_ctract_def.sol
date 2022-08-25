contract test_empty_ctract_def
{
}
abstract contract test_empty_abstract_ctract_def
{
}
contract test_body_abstract_ctract_def
{
    function simple_func () ;
}
abstract contract test_body_abstract_ctract_def
{
    function simple_func () ;
}
contract test_empty_inheritance_ctract_def is super
{
}
contract test_empty_inheritance_ctract_def is super.puper
{
}
contract test_empty_inheritance_ctract_def is super.puper , another.super.ctract
{
}
contract test_body_inheritance_ctract_def is super.puper , another.super.ctract
{
    function simple_func () ;
}
abstract contract test_empty_abstract_inheritance_ctract_def is super.puper
{
}
abstract contract test_body_abstract_inheritance_ctract_def is super.puper
{
    function simple_func () ;
}
