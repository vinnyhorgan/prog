Program TestMatsum
    Implicit None
    
    ! Dichiarazione variabili
    Integer, Parameter :: Max_Rows = 10
    Integer, Parameter :: Max_Cols = 10
    Real, Dimension(Max_Rows, Max_Cols) :: Matrix
    Integer :: Filled_Rows, Filled_Cols
    Real :: Total_Sum
    Integer :: i, j
    
    ! Inizializzazione: crea una matrice 5x7 all'interno dello spazio 10x10
    Filled_Rows = 5
    Filled_Cols = 7
    
    ! Riempie la matrice con valori di esempio
    Print *, "Riempimento della matrice ", Filled_Rows, "x", Filled_Cols
    Print *, ""
    
    Do i = 1, Filled_Rows
        Do j = 1, Filled_Cols
            Matrix(i, j) = Real(i * 10 + j)
        End Do
    End Do
    
    ! Stampa la matrice
    Print *, "Matrice:"
    Do i = 1, Filled_Rows
        Do j = 1, Filled_Cols
            Write(*, '(F6.1)', Advance='No') Matrix(i, j)
        End Do
        Print *, ""
    End Do
    Print *, ""
    
    ! Inizializza la somma
    Total_Sum = 0.0
    
    ! Chiama la subroutine per calcolare la somma
    Call Matsum(Matrix, Max_Rows, Max_Cols, Filled_Rows, Filled_Cols, Total_Sum)
    
    ! Stampa il risultato
    Print *, "Somma totale degli elementi: ", Total_Sum
    Print *, ""
    
    ! Secondo esempio: matrice più piccola 3x4
    Print *, "Secondo esempio con matrice 3x4:"
    Filled_Rows = 3
    Filled_Cols = 4
    
    Do i = 1, Filled_Rows
        Do j = 1, Filled_Cols
            Matrix(i, j) = Real(i + j)
        End Do
    End Do
    
    Print *, "Matrice:"
    Do i = 1, Filled_Rows
        Do j = 1, Filled_Cols
            Write(*, '(F6.1)', Advance='No') Matrix(i, j)
        End Do
        Print *, ""
    End Do
    Print *, ""
    
    Total_Sum = 0.0
    Call Matsum(Matrix, Max_Rows, Max_Cols, Filled_Rows, Filled_Cols, Total_Sum)
    
    Print *, "Somma totale degli elementi: ", Total_Sum
    
End Program TestMatsum

! Definizione della subroutine
Subroutine Matsum(Matrix, Rows, Cols, Filled_Rows, Filled_Cols, Sum)
    Implicit None
    Integer, Intent(In) :: Rows, Cols, Filled_Rows, Filled_Cols
    Real, Dimension(Rows, Cols), Intent(In) :: Matrix
    Real, Intent(Out) :: Sum
    Integer :: Row_Index, Col_Index
    
    Sum = 0.0
    
    Do Row_Index = 1, Filled_Rows
        Do Col_Index = 1, Filled_Cols
            Sum = Sum + Matrix(Row_Index, Col_Index)
        End Do
    End Do
    
End Subroutine Matsum