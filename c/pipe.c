#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>

#include <sys/types.h>
#include <sys/wait.h>


//char *newenviron[] = { NULL };
//char *newargv[] = { NULL };

int pipe(char* filePath,void** newargv,void** env){
  newargv[0] = filePath;
  int pipefd[2]; 
  pipe(pipefd);
  int pid = fork();
  if (pid == 0){
    close(pipefd[1]);
    execve(filePath,(char**)newargv,(char**)env);
  }else{
    close(pipefd[0]);
    return pid;
  }
}
