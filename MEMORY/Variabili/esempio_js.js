// STATIC: variabile globale/module-level - allocata staticamente
let counter = 0;

function mostraMemoria() {
    // STACK-DYNAMIC: variabili primitive locali
    let stackVar = 42;
    
    // HEAP-DYNAMIC IMPLICIT: oggetti e array sempre nell'heap
    // Garbage collector automatico
    let heapVar = { value: 100 };
    let heapArray = [1, 2, 3];
    
    counter++;
    
    console.log(`JS - Static: ${counter}, Stack: ${stackVar}, Heap: ${heapVar.value}`);
	
	// Mostra anche nel documento HTML
    document.body.innerHTML += `<p>JS - Static: ${counter}, Stack: ${stackVar}, Heap: ${heapVar.value}</p>`;
    
    // Non serve deallocazione - GC automatico
}

mostraMemoria();
mostraMemoria();