#include <sys/types.h>
#include <sys/wait.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

int* ret2c(){
  int* x = (int*)malloc(sizeof(int)*2);
  x[0] = 5;
  x[1] = 7;
  return x;
}

