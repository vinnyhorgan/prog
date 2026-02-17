-- INTERFACE

package Stack is
-- Public
	type Stack_Type is limited private;
	Max_Size : constant := 100;

	function Empty(Stk : in Stack_Type) return Boolean;
	procedure Push(	Stk : in out Stack_Type;
			Element : in Integer);
	procedure Pop(	Stk : in out Stack_Type);
	function Top(	Stk : in Stack_Type) return Integer;
-- Private
	private

		type List_Type is array (1..Max_Size) of Integer;
		type Stack_Type is
			record
			List : List_Type;
			Topsum : Integer range 0..Max_Size := 0;
			end record;
	
end Stack;

