type databuf =
	monitor
		const 
			bufsize = 100;
		var 
			buf : array [1 .. bufsize] of integer;
			next_in, next_out : 1 .. bufsize;
			filled : 0 .. bufsize;
			sender_q, receiver_q : queue;

		procedure entry put( item : integer );
		begin
			
			if filled = bufsize then delay(sender_q);
			
			buf[next_in] := item;
			next_in := (next_in mod bufsize) + 1;
			filled := filled + 1;
			continue(receiver_q);

		end;

		procedure entry get( var item : integer );
		begin

			if filled = 0 then delay(receiver_q);

			item := buf[next_out];
			next_out := (next_out mod bufsize) + 1;
			filled := filled - 1;
			continue(sender_q);

		end;

	begin
		filled := 0;
		next_in := 1;
		next_out := 1;
	end;
