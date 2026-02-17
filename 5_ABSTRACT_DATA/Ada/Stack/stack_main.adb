-- MAIN

with Stack, Ada.Text_IO, Ada.Integer_Text_IO;
use Stack, Ada.Text_IO;

procedure main is

Topone : Integer;
j : Integer;
MyStack : Stack_Type;

begin
	
	for j in 1 .. 10 loop
		Push(MyStack, j);
	end loop;

	while not Empty(MyStack) loop
		Topone := Top(MyStack);
		Ada.Integer_Text_IO.Put(Topone);
		Pop(MyStack);
	end loop;
	
end main;
