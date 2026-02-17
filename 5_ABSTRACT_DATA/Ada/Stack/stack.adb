-- BODY

with Ada.Text_IO; use Ada.Text_IO;

package body Stack is

	function Empty(Stk : in Stack_Type) return Boolean is
	begin
		return Stk.Topsum = 0;
	end Empty;

	procedure Push(	Stk : in out Stack_Type;
			Element : in Integer) is
	begin
		if Stk.Topsum >= Max_Size then
			Put_Line("ERROR - Stack overflow");
		else
			Stk.Topsum := Stk.Topsum + 1;
			Stk.List(Stk.Topsum) := Element;
		end if;
	end Push;

	procedure Pop(	Stk : in out Stack_Type) is
	begin
		if Stk.Topsum = 0 then
			Put_Line("ERROR - Stack underflow");
		else
			Stk.Topsum := Stk.Topsum - 1;
		end if;
	end Pop;

	function Top(	Stk : in Stack_Type) return Integer is
	begin
		if Stk.Topsum = 0 then
			Put_Line("ERROR - Stack is empty");
			return -1;
		else
			return Stk.List(Stk.Topsum);
		end if;
	end Top;

end Stack;
