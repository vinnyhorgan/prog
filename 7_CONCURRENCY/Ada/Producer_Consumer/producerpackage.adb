package body ProducerPackage is

task body Producer is
i : Integer;
begin
	loop

--		select
			accept produce ( v : in Integer) do
				i := v;
			end produce;
--		or
			accept get (v : out Integer) do
				v := i;
			end get;

--		end select;

	end loop;
end Producer;

end ProducerPackage;
