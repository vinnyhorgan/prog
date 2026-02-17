// Dichiarazione del delegate
public delegate void MioDelegate(string messaggio);

// Metodi compatibili
public void StampaConsole(string testo)
{
    Console.WriteLine(testo);
}

public void StampaUpperCase(string testo)
{
    Console.WriteLine(testo.ToUpper());
}

// Utilizzo
MioDelegate del = StampaConsole;
del("Ciao!"); // Output: Ciao!

del = StampaUpperCase;
del("Ciao!"); // Output: CIAO!

//Multicast Delegate

MioDelegate del = StampaConsole;
del += StampaUpperCase; // Aggiunge un altro metodo
del("Test"); // Esegue entrambi i metodi