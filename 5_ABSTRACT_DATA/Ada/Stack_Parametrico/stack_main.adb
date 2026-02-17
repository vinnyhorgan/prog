-- MAIN

with Generic_Stack, Ada.Text_IO, Ada.Integer_Text_IO, Ada.Float_Text_IO;
use Ada.Text_IO, Ada.Integer_Text_IO, Ada.Float_Text_IO;

procedure main is

package Integer_Stack is new Generic_Stack(100, Integer); 
package Float_Stack is new Generic_Stack(100, Float);

i : Integer;
f : Float;
MyIntStack : Integer_Stack.Stack_Type;
MyFloatStack : Float_Stack.Stack_Type;

begin

	Integer_Stack.Push(MyIntStack, 21);
	i :=  Integer_Stack.Top(MyIntStack) ;
	Ada.Integer_Text_IO.Put( i );
	Integer_Stack.Pop(MyIntStack);

	Float_Stack.Push(MyFloatStack, 41.3);
	f := Float_Stack.Top(MyFloatStack);
	Ada.Float_Text_IO.Put( f );
	Float_Stack.Pop(MyFloatStack) ;
	
end main;
