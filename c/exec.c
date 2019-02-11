#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>

// I realize this code is ugly

int* cexec(char* filePath,void** newargv,void** env,int pipeO,int pipeE){
  newargv[0] = filePath;
  if(pipeO){
    if(pipeE){ // pipe both
      int pipeOfd[2]; 
      pipe(pipeOfd);
      int  pipeEfd[2]; 
      pipe(pipeEfd);
      int pid = fork();
      if (pid == 0){ //child
        close(pipeOfd[0]);
        dup2( pipeOfd[1],1);
        close(pipeEfd[0]);
        dup2( pipeEfd[1],2);
        execve(filePath,(char**)newargv,(char**)env);
      }else{ //parent
        close(pipeOfd[1]);
        close(pipeEfd[1]);
        int* ret = (int*)malloc(sizeof(int)*3);
        ret[0] = pid;
        ret[1] = pipeOfd[0];
        ret[2] = pipeEfd[0];
        return ret;
      }
    }else{
      int pipeOfd[2]; 
      pipe(pipeOfd);
      int pid = fork();
      if (pid == 0){ //child
        close(pipeOfd[0]);
        dup2( pipeOfd[1],1);
        execve(filePath,(char**)newargv,(char**)env);
      }else{ //parent
        close(pipeOfd[1]);
        int* ret = (int*)malloc(sizeof(int)*2);
        ret[0] = pid;
        ret[1] = pipeOfd[0];
        return ret;
      }
    }
  }else{
    if(pipeE){
      int  pipeEfd[2]; 
      pipe(pipeEfd);
      int pid = fork();
      if (pid == 0){ //child
        close(pipeEfd[0]);
        dup2( pipeEfd[1],2);
        execve(filePath,(char**)newargv,(char**)env);
      }else{ //parent
        close(pipeEfd[1]);
        int* ret = (int*)malloc(sizeof(int)*2);
        ret[0] = pid;
        ret[1] = pipeEfd[0];
        return ret;
      }
    }else{
      int pid = fork();
      if (pid == 0){ //child
        execve(filePath,(char**)newargv,(char**)env);
      }else{ //parent
        int* ret = (int*)malloc(sizeof(int)*1);
        ret[0] = pid;
        return ret;
      }
    }
  }
}
