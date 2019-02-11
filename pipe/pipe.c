#include <sys/types.h>
#include <sys/wait.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

int pipeC(char* str){
   int pipefd[2];
   pid_t cpid;
   pipe(pipefd);
   cpid = fork();
   if (cpid == 0) {    /* Child reads from pipe */
       close(pipefd[0]);          /* Close unused read end */
       write(pipefd[1], str, strlen(str));
       close(pipefd[1]);          /* Reader will see EOF */
       exit(EXIT_SUCCESS);
   } else {            /* Parent writes argv[1] to pipe */
       close(pipefd[1]);          /* Close unused write end */
       return pipefd[0];
       wait(NULL);                /* Wait for child */
   }
}

