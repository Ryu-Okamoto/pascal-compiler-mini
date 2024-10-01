program testBasic;
var i: integer;
    $: array[1..10] of integer;     { <<<< lexical error here }

begin
    i := 11;
    $[5] := i;
    writeln($[5]);
end.