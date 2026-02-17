with Ada.Text_IO, Ada.Integer_Text_IO;

package body ConsumerPackage is
	task body Consumer is
	myp : Producer_ptr;
	myid : Integer;
	v : Integer;
		begin
		accept start( p : in Producer_ptr; id : in Integer) do
			myp := p;
			myid := id;
		end start;
		loop
			Ada.Integer_Text_IO.Put( myid );
			Ada.Text_IO.Put_Line(" sending production request");
			myp.produce(myid);
			delay (myid * 0.001);
			myp.get(v);
			Ada.Integer_Text_IO.Put( v );
			Ada.Text_IO.Put(" dispatched for ");
			Ada.Integer_Text_IO.Put( myid );
			Ada.Text_Io.Put_Line(", continuing computation");
			delay (myid * 0.001);
		end loop;
	end Consumer;
end ConsumerPackage;
	
