#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>

int h_wait(int pid){
  int wstatus=0;
  waitpid(pid, &wstatus, 0);
  return wstatus;
}
