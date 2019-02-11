#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>

int* cexec(char* filePath,void** newargv,void** env,int pipeO,int pipeE,int pipeI,int pipeIfd){
  newargv[0] = filePath; // set the first arg to the fileThat will be run
  int pipeOfd[2]; 
  int pipeEfd[2]; 
  if(pipeO) //if pipeing stdout do so
    pipe(pipeOfd);
  if(pipeE) // if piping stderr do so
    pipe(pipeEfd);
  int pid = fork(); 
  if(pid){ //parent
    if(pipeO) // close read end
      close(pipeOfd[1]); 
    if(pipeE)
      close(pipeEfd[1]); 
    int* ret = (int*)malloc(sizeof(int)*(1+pipeO+pipeE));// ret wil have the pid and then any pipes with out before err if both
    ret[0] = pid;
    if(pipeO)
      ret[1] = pipeOfd[0];
    if(pipeE)
      ret[1+pipeO] = pipeEfd[0]; // put in [2] if out is in 1
    return ret;
  }else{ //child
    if(pipeO){
      close(pipeOfd[0]);
      dup2( pipeOfd[1],1); // copy pipe into std out
    }
    if(pipeE){
      close(pipeEfd[0]);
      dup2( pipeEfd[1],2); // copy pipe into std err
    }
    if(pipeI)
      dup2(pipeIfd,0); // copy stdin into existing in-pipe
    execve(filePath,(char**)newargv,(char**)env);
  }
}
