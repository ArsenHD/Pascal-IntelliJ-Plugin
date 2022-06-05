program functionDeclaration;

function foo(x: Real, y: Real): Real
var myVar: Real;
begin
    myVar := x * x + y * y;
    foo := sqrt(myVar);
end;

begin
    WriteLn(foo(3.0, 4.0));
end.
