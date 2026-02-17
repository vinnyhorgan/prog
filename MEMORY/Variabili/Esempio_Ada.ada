with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

procedure Esempio_Ada is
   -- STATIC: variabile a livello package - allocata staticamente
   Counter : Integer := 0;
   
   procedure Mostra_Memoria is
      -- STACK-DYNAMIC: variabile locale
      Stack_Var : Integer := 42;
      
      -- HEAP-DYNAMIC EXPLICIT: usando access types
      type Int_Ptr is access Integer;
      procedure Free is new Ada.Unchecked_Deallocation(Integer, Int_Ptr);
      
      Heap_Var : Int_Ptr := new Integer'(100);
   begin
      Counter := Counter + 1;
      
      Put_Line("Ada - Static:" & Integer'Image(Counter) &
               ", Stack:" & Integer'Image(Stack_Var) &
               ", Heap:" & Integer'Image(Heap_Var.all));
      
      -- Deallocazione esplicita
      Free(Heap_Var);
   end Mostra_Memoria;
   
begin
   Mostra_Memoria;
   Mostra_Memoria;
end Esempio_Ada;