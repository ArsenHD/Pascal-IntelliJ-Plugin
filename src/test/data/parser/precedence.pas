program precedence;

var b: Boolean;
begin
    b := 1 = 1;
    b := 1 <> 2;
    b := (1 + 1) = (2);
    b := 1 + 2 * 3;
    b := not True and False or True and True
end.
