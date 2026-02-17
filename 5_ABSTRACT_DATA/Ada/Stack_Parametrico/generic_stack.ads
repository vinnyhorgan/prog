-- INTERFACE

generic

	Max_Size : Positive;
	type Element_Type is private;

package Generic_Stack is
-- Public
	type Stack_Type is limited private;

	function Empty(Stk : in Stack_Type) return Boolean;
	procedure Push(	Stk : in out Stack_Type;
					Element : in Element_Type);
	procedure Pop(	Stk : in out Stack_Type);
	function Top(	Stk : in Stack_Type) return Element_Type;
-- Private
	private

		type List_Type is array (1..Max_Size) of Element_Type;
		type Stack_Type is
			record
			List : List_Type;
			Topsum : Integer range 0..Max_Size := 0;
			end record;
	
end Generic_Stack;

