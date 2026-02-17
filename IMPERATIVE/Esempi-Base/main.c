#include <stdio.h>
#include <stdlib.h>
int main(){
  printf("Hello world!\n");
  int a[] = {1, 2, 3, 4};
  printf("%d\n", a[3]);
  char stringa[] = {'D', 'y', 'l', 'a', 'n'};
  printf("%d\n", stringa[2]);
  int x = 42;
  printf("%d\n", x);
  int* p = &x;
  int* q = p;
  int y = *q;
  printf("%d\n", &p);
  int *z = (int*) malloc (10* sizeof(int));
  *z = 1;
  *(z+1) = 2;
  *(z+3) = 3;
  //int u = &z;
  printf("%d\n", *(z));
}