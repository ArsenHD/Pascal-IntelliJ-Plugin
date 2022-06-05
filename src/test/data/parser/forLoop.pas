program forLoop;

begin
    for i := 1 to 10 do
        WriteLn(i);

    for i := -10 to 10 do begin
        WriteLn(i * i);
        WriteLn(i + i)
    end;

    for i := 100 downto 1 do
        WriteLn(i);
end.
