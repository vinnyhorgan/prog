type producer = process(buffer : databuf);
	var newvalue : integer;
	begin
		cycle
			-- produce newvalue --
			buffer.put(newvalue);
		end
	end;

type consumer = process(buffer : databuf);
	var stored_value : integer;
	begin
		cycle
			buffer.get(stored_value);
			-- consume stored_value --
		end
	end;
