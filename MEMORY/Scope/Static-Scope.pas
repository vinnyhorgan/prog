program ScopeStaticoEsempio;

procedure A;
var x: integer;
    
    procedure B;
    var y: integer;
        
        procedure C;
        begin
            writeln('Valore di x in C: ', x);  // Accede a x di A
            writeln('Valore di y in C: ', y);  // Accede a y di B
        end;
        
    begin
        y := 20;
        writeln('B chiama C');
        C();  // B chiama C
    end;
    
begin
    x := 10;
    writeln('A chiama B');
    B();  // A chiama B
end;

begin
    A();  // Main chiama A
end.
