program testBasic;
var f: boolean;

begin
    f := true;
    if f = true then
    begin
        writeln('then);  { <<<< lexical error here }
    end;
end.
