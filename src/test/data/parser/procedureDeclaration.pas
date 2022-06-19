program procedureDeclaration;

procedure bar(x: Integer)
var myVar: Integer;
begin
    myVar := 2 * x;
    WriteLn("myVar " + myVar);
end;

begin
    bar(10)
end.
