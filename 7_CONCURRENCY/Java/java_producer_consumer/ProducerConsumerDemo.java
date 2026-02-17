import java.util.LinkedList;
import java.util.Queue;

// Classe Buffer condiviso tra produttori e consumatori
class Buffer {
    private Queue<Integer> queue;
    private int capacita;
    
    public Buffer(int capacita) {
        this.queue = new LinkedList<>();
        this.capacita = capacita;
    }
    
    // Metodo per produrre (inserire) un elemento
    public synchronized void produci(int elemento) throws InterruptedException {
        // Aspetta finché il buffer è pieno
        while (queue.size() == capacita) {
            System.out.println("Buffer PIENO! Produttore aspetta...");
            wait();
        }
        
        // Inserisce l'elemento
        queue.add(elemento);
        System.out.println("Prodotto: " + elemento + " | Buffer size: " + queue.size());
        
        // Notifica i consumatori che c'è un nuovo elemento
        notifyAll();
    }
    
    // Metodo per consumare (prelevare) un elemento
    public synchronized int consuma() throws InterruptedException {
        // Aspetta finché il buffer è vuoto
        while (queue.isEmpty()) {
            System.out.println("Buffer VUOTO! Consumatore aspetta...");
            wait();
        }
        
        // Preleva l'elemento
        int elemento = queue.remove();
        System.out.println("Consumato: " + elemento + " | Buffer size: " + queue.size());
        
        // Notifica i produttori che c'è spazio disponibile
        notifyAll();
        
        return elemento;
    }
}

// Classe Produttore
class Produttore implements Runnable {
    private Buffer buffer;
    private int id;
    
    public Produttore(Buffer buffer, int id) {
        this.buffer = buffer;
        this.id = id;
    }
    
    @Override
    public void run() {
        try {
            for (int i = 1; i <= 5; i++) {
                int elemento = id * 100 + i; // Genera un numero univoco
                buffer.produci(elemento);
                Thread.sleep(500); // Simula tempo di produzione
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }
}

// Classe Consumatore
class Consumatore implements Runnable {
    private Buffer buffer;
    private int id;
    
    public Consumatore(Buffer buffer, int id) {
        this.buffer = buffer;
        this.id = id;
    }
    
    @Override
    public void run() {
        try {
            for (int i = 1; i <= 5; i++) {
                int elemento = buffer.consuma();
                Thread.sleep(800); // Simula tempo di elaborazione
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }
}

// Main class
public class ProducerConsumerDemo {
    public static void main(String[] args) {
        System.out.println("=== PRODUTTORE-CONSUMATORE ===\n");
        
        // Crea un buffer con capacità 3
        Buffer buffer = new Buffer(3);
        
        // Crea 2 produttori
        Thread produttore1 = new Thread(new Produttore(buffer, 1), "Produttore-1");
        Thread produttore2 = new Thread(new Produttore(buffer, 2), "Produttore-2");
        
        // Crea 2 consumatori
        Thread consumatore1 = new Thread(new Consumatore(buffer, 1), "Consumatore-1");
        Thread consumatore2 = new Thread(new Consumatore(buffer, 2), "Consumatore-2");
        
        // Avvia tutti i thread
        produttore1.start();
        produttore2.start();
        consumatore1.start();
        consumatore2.start();
        
        // Aspetta che tutti i thread terminino
        try {
            produttore1.join();
            produttore2.join();
            consumatore1.join();
            consumatore2.join();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        
        System.out.println("\n=== FINE ===");
    }
}
