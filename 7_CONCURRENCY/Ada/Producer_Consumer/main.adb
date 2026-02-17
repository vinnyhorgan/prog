with ProducerPackage, ConsumerPackage, Ada.Text_IO, Ada.Integer_Text_IO;
use ProducerPackage, ConsumerPackage, Ada.Text_IO;

procedure main is

myprod : Producer_ptr;
cons : array (1 .. 100) of Consumer_ptr;

begin

	Put_Line("starting");

	myprod := new Producer;

	for i in 1 .. 100 loop
		cons(i) := new Consumer;
		cons(i).start(myprod,i);
	end loop;

	--myprod.produce(0);
	--myprod.get(i);

	--Ada.Integer_Text_IO.Put(i);

	Put_Line(" ending");

end main;
