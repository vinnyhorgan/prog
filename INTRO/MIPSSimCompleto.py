#!/usr/bin/env python3
"""
Simulazione dell'operazione di somma in memoria
Equivalente al codice macchina MIPS mostrato precedentemente
"""

def main():
    # Simulazione della memoria come dizionario
    # Le chiavi sono gli indirizzi, i valori sono i contenuti
    memory = {}
    
    print("=== SIMULAZIONE OPERAZIONE MEMORIA ===")
    print("Operazione: Somma valori in locazioni 10 e 11, risultato in locazione 12\n")
    
    # Inizializzazione della memoria con alcuni valori di esempio
    memory[10] = 25    # Valore nella locazione 10
    memory[11] = 17    # Valore nella locazione 11
    
    print("STATO INIZIALE DELLA MEMORIA:")
    print(f"Locazione 10: {memory[10]}")
    print(f"Locazione 11: {memory[11]}")
    print(f"Locazione 12: {memory.get(12, 'non inizializzata')}\n")
    
    # Simulazione delle istruzioni MIPS:
    # lw $t0, 10($zero)    - Carica dalla locazione 10
    t0 = memory[10]
    print(f"1. LOAD: t0 = memory[10] = {t0}")
    
    # lw $t1, 11($zero)    - Carica dalla locazione 11  
    t1 = memory[11]
    print(f"2. LOAD: t1 = memory[11] = {t1}")
    
    # add $t2, $t0, $t1    - Somma i valori
    t2 = t0 + t1
    print(f"3. ADD:  t2 = t0 + t1 = {t0} + {t1} = {t2}")
    
    # sw $t2, 12($zero)    - Memorizza nella locazione 12
    memory[12] = t2
    print(f"4. STORE: memory[12] = t2 = {t2}\n")
    
    print("STATO FINALE DELLA MEMORIA:")
    print(f"Locazione 10: {memory[10]}")
    print(f"Locazione 11: {memory[11]}")
    print(f"Locazione 12: {memory[12]}")
    
    # Verifica del risultato
    print(f"\nVerifica: {memory[10]} + {memory[11]} = {memory[12]} ✓")


def interactive_version():
    """Versione interattiva che permette all'utente di inserire i valori"""
    print("\n" + "="*50)
    print("VERSIONE INTERATTIVA")
    print("="*50)
    
    memory = {}
    
    try:
        # Input dell'utente
        val10 = int(input("Inserisci il valore per la locazione 10: "))
        val11 = int(input("Inserisci il valore per la locazione 11: "))
        
        memory[10] = val10
        memory[11] = val11
        
        print(f"\nEsecuzione dell'operazione...")
        
        # Simulazione delle istruzioni
        t0 = memory[10]
        t1 = memory[11] 
        t2 = t0 + t1
        memory[12] = t2
        
        print(f"Risultato: memory[10] + memory[11] = {val10} + {val11} = {memory[12]}")
        
    except ValueError:
        print("Errore: Inserire solo numeri interi")


class MemorySimulator:
    """Classe per simulare la memoria e le operazioni"""
    
    def __init__(self):
        self.memory = {}
        self.registers = {
            't0': 0,
            't1': 0, 
            't2': 0
        }
    
    def load_word(self, register, address):
        """Simula l'istruzione lw (load word)"""
        if address not in self.memory:
            print(f"Warning: Locazione {address} non inizializzata, assumo 0")
            self.memory[address] = 0
        
        self.registers[register] = self.memory[address]
        print(f"lw ${register}, {address}: ${register} = {self.registers[register]}")
    
    def add_registers(self, dest, src1, src2):
        """Simula l'istruzione add"""
        result = self.registers[src1] + self.registers[src2]
        self.registers[dest] = result
        print(f"add ${dest}, ${src1}, ${src2}: ${dest} = {result}")
    
    def store_word(self, register, address):
        """Simula l'istruzione sw (store word)"""
        self.memory[address] = self.registers[register]
        print(f"sw ${register}, {address}: memory[{address}] = {self.registers[register]}")
    
    def set_memory(self, address, value):
        """Imposta un valore in memoria"""
        self.memory[address] = value
    
    def get_memory(self, address):
        """Ottiene un valore dalla memoria"""
        return self.memory.get(address, 0)
    
    def print_state(self):
        """Stampa lo stato di registri e memoria"""
        print("\nSTATO REGISTRI:")
        for reg, val in self.registers.items():
            print(f"  ${reg}: {val}")
        
        print("\nSTATO MEMORIA:")
        for addr in sorted(self.memory.keys()):
            print(f"  Locazione {addr}: {self.memory[addr]}")


def object_oriented_version():
    """Versione object-oriented della simulazione"""
    print("\n" + "="*50)
    print("VERSIONE OBJECT-ORIENTED")
    print("="*50)
    
    # Crea il simulatore
    sim = MemorySimulator()
    
    # Inizializza la memoria
    sim.set_memory(10, 42)
    sim.set_memory(11, 58)
    
    print("Esecuzione del programma MIPS:")
    print("-" * 30)
    
    # Esegue le istruzioni
    sim.load_word('t0', 10)      # lw $t0, 10($zero)
    sim.load_word('t1', 11)      # lw $t1, 11($zero)
    sim.add_registers('t2', 't0', 't1')  # add $t2, $t0, $t1
    sim.store_word('t2', 12)     # sw $t2, 12($zero)
    
    # Mostra lo stato finale
    sim.print_state()


if __name__ == "__main__":
    # Esegue tutte le versioni
    main()
    interactive_version()
    object_oriented_version()