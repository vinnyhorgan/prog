#include <iostream>
#include <string>

struct Persona {
    std::string nome;
    int eta;

    // Costruttore di default
    Persona() {
        std::cout << "Costruttore di default chiamato" << std::endl;
        nome = "";
        eta = 0;
    }

    // Costruttore parametrizzato
    Persona(std::string n, int e) : nome(n), eta(e) {
        std::cout << "Costruttore parametrizzato chiamato" << std::endl;
    }
};

int main() {
    Persona p1; // Chiama il costruttore di default
    Persona p2("Mario", 30); // Chiama il costruttore parametrizzato

    std::cout << "Nome: " << p1.nome << ", Età: " << p1.eta << std::endl;
    std::cout << "Nome: " << p2.nome << ", Età: " << p2.eta << std::endl;

    return 0;
}