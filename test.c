#include <stdio.h>
#include <stdlib.h>

/*
	nasm -f elf f.asm
	gcc -m32 -c g.c
	gcc -m32 f.o g.o test.c -o test
	./test
*/

int f(int a, int b, int c, int d, int e);
int g(int a, int b, int c, int d, int e);

int main(void) {
  int i;
  int a,b,c,d,e;
  int v1, v2;
  
  srand(time(0));
  
  for(i = 0; i < 100; i++) {
    a = rand()%5000-1000;
    b = rand()%5000-1000;
    c = rand()%5000-1000;
    d = rand()%5000-1000;
    e = rand()%5000-1000;
    
    printf("%d %d %d %d %d\n",a,b,c,d,e);
    v1=f(a,b,c,d,e);
    printf("%d\n", v1);
    v2=g(a,b,c,d,e);
    printf("%d\n", v2);
    if(v1 != v2) {
      return EXIT_FAILURE;
    }
  }
  
  return EXIT_SUCCESS;
}
