
$runtime = (\Rehack\GlobalObject::get() as dynamic)->jsoo_runtime;
$call1 = $runtime["caml_call1"];
$cst_hello_world = $runtime["caml_new_string"]("hello world");
$Stdlib = Stdlib::get();

$call1($Stdlib[46], $cst_hello_world);

$Hello_world = Vector{0} as dynamic;

return($Hello_world);
