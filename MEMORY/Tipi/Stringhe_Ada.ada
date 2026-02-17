with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Bounded;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure String_Types_Demo is

   -- 1. STRINGA STATICA (String)
   -- Lunghezza fissa, determinata a compile-time
   Nome : String(1..10) := "Alessandro";
   Saluto : String := "Ciao!";  -- Lunghezza inferita (5 caratteri)
   
   -- 2. STRINGA A LUNGHEZZA DINAMICA LIMITATA (Bounded_String)
   -- Lunghezza massima fissa, contenuto dinamico
   package String_50 is new Ada.Strings.Bounded.Generic_Bounded_Length(Max => 50);
   use String_50;
   
   Messaggio : Bounded_String;
   
   -- 3. STRINGA DINAMICA (Unbounded_String)
   -- Lunghezza completamente dinamica
   Testo : Unbounded_String;
   
begin
   Put_Line("=== STRINGHE STATICHE ===");
   Put_Line("Nome: " & Nome);
   Put_Line("Lunghezza: " & Integer'Image(Nome'Length));
   Put_Line("Saluto: " & Saluto);
   New_Line;
   
   -- Le stringhe statiche hanno lunghezza fissa
   -- Nome := "Bob";  -- ERRORE: lunghezze incompatibili
   -- Dobbiamo riempire con spazi:
   Nome := "Bob       ";
   Put_Line("Nome modificato: " & Nome);
   New_Line;
   
   Put_Line("=== STRINGHE BOUNDED (limitate) ===");
   -- Operazioni con bounded strings
   Messaggio := To_Bounded_String("Questo è un messaggio bounded");
   Put_Line("Messaggio: " & To_String(Messaggio));
   Put_Line("Lunghezza: " & Integer'Image(Length(Messaggio)));
   Put_Line("Lunghezza massima: " & Integer'Image(Max_Length));
   
   -- Possiamo cambiare il contenuto entro il limite
   Messaggio := To_Bounded_String("Breve");
   Put_Line("Messaggio modificato: " & To_String(Messaggio));
   
   -- Concatenazione
   Append(Messaggio, " - aggiunto testo");
   Put_Line("Dopo append: " & To_String(Messaggio));
   New_Line;
   
   Put_Line("=== STRINGHE UNBOUNDED (dinamiche) ===");
   -- Operazioni con unbounded strings
   Testo := To_Unbounded_String("Ada è un linguaggio potente");
   Put_Line("Testo: " & To_String(Testo));
   Put_Line("Lunghezza: " & Integer'Image(Length(Testo)));
   
   -- Concatenazione semplice
   Testo := Testo & " e sicuro!";
   Put_Line("Dopo concatenazione: " & To_String(Testo));
   
   -- Operazioni di manipolazione
   Append(Testo, " Supporta stringhe di qualsiasi lunghezza.");
   Put_Line("Testo finale: " & To_String(Testo));
   Put_Line("Lunghezza finale: " & Integer'Image(Length(Testo)));
   New_Line;
   
   Put_Line("=== CONFRONTO ===");
   Put_Line("Statiche:  lunghezza fissa, efficienza massima");
   Put_Line("Bounded:   limite superiore, overhead minimo");
   Put_Line("Unbounded: completamente flessibili, heap allocation");
   
end String_Types_Demo;