#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/wait.h>


//char *newenviron[] = { NULL };
//char *newargv[] = { NULL };

int exec(char* filePath,void** newargv,void** env){
  newargv[0] = filePath;
  int pid = fork();
  if (pid == 0){
    execve(filePath,(char**)newargv,(char**)env);
  }else{
    return pid;
  }
}
