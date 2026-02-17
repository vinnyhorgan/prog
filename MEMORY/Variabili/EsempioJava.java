public class EsempioJava {
    // STATIC: variabile di classe - allocata nell'area method/static
    private static int counter = 0;
    
    public static void mostraMemoria() {
        // STACK-DYNAMIC: variabili primitive locali
        int stackVar = 42;
        
        // HEAP-DYNAMIC IMPLICIT: oggetti sempre allocati nell'heap
        // Garbage collector si occupa della deallocazione
        Integer heapVar = new Integer(100);
        StringBuilder builder = new StringBuilder("heap");
        
        counter++;
        
        System.out.println("Java - Static: " + counter + 
                         ", Stack: " + stackVar + 
                         ", Heap: " + heapVar);
        
        // Non serve deallocazione esplicita - GC automatico
    }
    
    public static void main(String[] args) {
        mostraMemoria();
        mostraMemoria();
    }
}