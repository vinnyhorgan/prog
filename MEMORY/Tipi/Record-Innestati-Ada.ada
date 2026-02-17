with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Fixed;

procedure Record_Demo is

   -- Record completo Dipendente con struttura innestata (stile COBOL)
   type Dipendente_Type is record
      Matricola : Integer;
      Anagrafici : record
         Cognome : String(1..20);
         Nome : String(1..20);
         Data_Nascita : record
            Giorno : Integer range 1..31;
            Mese : Integer range 1..12;
            Anno : Integer range 1900..2100;
         end record;
      end record;
      Lavorativi : record
         Dipartimento : String(1..15);
         Stipendio : Float;
         Anni_Servizio : Integer;
      end record;
   end record;

   -- Record Prodotto
   type Prodotto_Type is record
      Codice_Prod : String(1..10);
      Descrizione : String(1..30);
      Prezzo : Float;
      Quantita : Integer;
      Valore_Totale : Float;
   end record;

   -- Variabili
   Dip : Dipendente_Type;
   Prod : Prodotto_Type;
   Aumento : Float;

   -- Funzione ausiliaria per riempire stringhe con spazi
   function Pad_String(S : String; Length : Positive) return String is
      Result : String(1..Length) := (others => ' ');
   begin
      if S'Length <= Length then
         Result(1..S'Length) := S;
      else
         Result := S(S'First..S'First + Length - 1);
      end if;
      return Result;
   end Pad_String;

   procedure Inizializza_Dipendente is
   begin
      Dip.Matricola := 123456;
      Dip.Anagrafici.Cognome := Pad_String("ROSSI", 20);
      Dip.Anagrafici.Nome := Pad_String("MARIO", 20);
      Dip.Anagrafici.Data_Nascita.Giorno := 15;
      Dip.Anagrafici.Data_Nascita.Mese := 3;
      Dip.Anagrafici.Data_Nascita.Anno := 1985;
      Dip.Lavorativi.Dipartimento := Pad_String("INFORMATICA", 15);
      Dip.Lavorativi.Stipendio := 35000.00;
      Dip.Lavorativi.Anni_Servizio := 10;
   end Inizializza_Dipendente;

   procedure Mostra_Dipendente is
   begin
      Put_Line("=== DATI DIPENDENTE ===");
      Put("Matricola: "); Put(Dip.Matricola, 0); New_Line;
      Put_Line("Nome completo: " & Ada.Strings.Fixed.Trim(Dip.Anagrafici.Nome, Ada.Strings.Right) & 
               " " & Ada.Strings.Fixed.Trim(Dip.Anagrafici.Cognome, Ada.Strings.Right));
      Put("Data di nascita: ");
      Put(Dip.Anagrafici.Data_Nascita.Giorno, 0);
      Put("/");
      Put(Dip.Anagrafici.Data_Nascita.Mese, 0);
      Put("/");
      Put(Dip.Anagrafici.Data_Nascita.Anno, 0);
      New_Line;
      Put_Line("Dipartimento: " & Ada.Strings.Fixed.Trim(Dip.Lavorativi.Dipartimento, Ada.Strings.Right));
      Put("Stipendio: EUR "); Put(Dip.Lavorativi.Stipendio, Fore => 1, Aft => 2, Exp => 0); New_Line;
      Put("Anni di servizio: "); Put(Dip.Lavorativi.Anni_Servizio, 0); New_Line;
   end Mostra_Dipendente;

   procedure Aggiorna_Stipendio is
   begin
      Aumento := Dip.Lavorativi.Stipendio * 0.05;
      Dip.Lavorativi.Stipendio := Dip.Lavorativi.Stipendio + Aumento;
      
      New_Line;
      Put_Line("Aumento del 5% applicato");
      Put("Nuovo stipendio: EUR "); Put(Dip.Lavorativi.Stipendio, Fore => 1, Aft => 2, Exp => 0); New_Line;
   end Aggiorna_Stipendio;

   procedure Inizializza_Prodotto is
   begin
      Prod.Codice_Prod := Pad_String("PROD-001", 10);
      Prod.Descrizione := Pad_String("Laptop Dell XPS 15", 30);
      Prod.Prezzo := 1299.99;
      Prod.Quantita := 25;
      Prod.Valore_Totale := 0.0;
   end Inizializza_Prodotto;

   procedure Mostra_Prodotto is
   begin
      Put_Line("=== DATI PRODOTTO ===");
      Put_Line("Codice: " & Ada.Strings.Fixed.Trim(Prod.Codice_Prod, Ada.Strings.Right));
      Put_Line("Descrizione: " & Ada.Strings.Fixed.Trim(Prod.Descrizione, Ada.Strings.Right));
      Put("Prezzo unitario: EUR "); Put(Prod.Prezzo, Fore => 1, Aft => 2, Exp => 0); New_Line;
      Put("Quantita' in magazzino: "); Put(Prod.Quantita, 0); New_Line;
   end Mostra_Prodotto;

   procedure Calcola_Valore is
   begin
      Prod.Valore_Totale := Prod.Prezzo * Float(Prod.Quantita);
      Put("Valore totale magazzino: EUR "); Put(Prod.Valore_Totale, Fore => 1, Aft => 2, Exp => 0); New_Line;
   end Calcola_Valore;

begin
   Inizializza_Dipendente;
   Mostra_Dipendente;
   Aggiorna_Stipendio;
   
   New_Line;
   Put_Line("================================");
   New_Line;
   
   Inizializza_Prodotto;
   Mostra_Prodotto;
   Calcola_Valore;
end Record_Demo