program repeatUntil;

var x: Integer;

begin
    x := 0;
    repeat
        x := x + 1;
        WriteLn(x)
    until x = 10;
end.
