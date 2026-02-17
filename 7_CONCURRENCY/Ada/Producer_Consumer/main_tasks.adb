with Ada.Text_IO, Ada.Integer_Text_IO;
use Ada.Text_IO;

procedure main is

task Printer is
end Printer;

task type Monit is
end Monit;

task body Printer is
begin
	loop
		Put_Line("I'm a printer");
		delay 0.3;
	end loop;

end Printer;

task body Monit is
begin
	loop
		Put_Line("I'm a monit");
		delay 0.5;
	end loop;

end Monit;

mymonit : Monit;

begin

	Put_Line("starting");

	Put_Line(" ending");

end main;
