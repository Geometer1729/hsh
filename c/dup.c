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

int* cexec(char* filePath,void** newargv,void** env){
  newargv[0] = filePath;
  int pipefd[2]; 
  pipe(pipefd);
  int pid = fork();
  if (pid == 0){ //child
    close(pipefd[0]);
    dup2(pipefd[1],1);
    execve(filePath,(char**)newargv,(char**)env);
  }else{ //parent
    close(pipefd[1]);
    int* ret = (int*)malloc(sizeof(int)*2);
    ret[0] = pid;
    ret[1] = pipefd[0];
    return ret;
  }
}
