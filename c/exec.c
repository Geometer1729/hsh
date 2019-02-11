#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>

int* cexec(char* filePath,void** newargv,void** env,int pipeO,int pipeE){
  newargv[0] = filePath;
  int pipeOfd[2]; 
  int  pipeEfd[2]; 
  if(pipeO)
    pipe(pipeOfd);
  if(pipeE)
    pipe(pipeEfd);
  int pid = fork();
  if(pid == 0){ //child
    if(pipeO){
      close(pipeOfd[0]);
      dup2( pipeOfd[1],1);
    }
    if(pipeE){
      close(pipeEfd[0]);
      dup2( pipeEfd[1],2);
    }
    execve(filePath,(char**)newargv,(char**)env);
  }else{ //parent
    if(pipeO)
      close(pipeOfd[1]);
    if(pipeE)
      close(pipeEfd[1]);
    int* ret = (int*)malloc(sizeof(int)*(1+pipeO+pipeE));
    ret[0] = pid;
    if(pipeO)
      ret[1] = pipeOfd[0];
    if(pipeE)
      ret[1+pipeO] = pipeEfd[0];
    return ret;
  }
}
