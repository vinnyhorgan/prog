def memory_operation():
    """Simula l'operazione di somma in memoria"""
    # Simulazione della memoria
    memory = {}
    
    # Inizializza le locazioni 10 e 11
    memory[10] = 25
    memory[11] = 17
    
    # Somma i valori e memorizza in locazione 12
    memory[12] = memory[10] + memory[11]
    
    return memory

def main():
    """Funzione principale"""
    # Esegue l'operazione di memoria
    memory = memory_operation()
    
    # Mostra i risultati
    print(f"Locazione 10: {memory[10]}")
    print(f"Locazione 11: {memory[11]}")
    print(f"Somma (locazione 12): {memory[12]}")

if __name__ == "__main__":
    main()