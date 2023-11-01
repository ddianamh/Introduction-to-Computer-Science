#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

static void action(int m, int n)
{
    /*
    
    */
    printf("(%d, %d)\n", m, n);
    if( n>0 )
    {
        pid_t pid;

        pid = fork();

        if( pid == 0)
        {
            printf("%d: child\n", getpid());
            action(m, n-1);
            //printf("%d: exiting\n", getpid());
            //exit(0);
            
        }
        else
            printf("%d: parent\n", getpid());
    }
}
//monday 11 15 r1 room 87
int main( int argc, char *argv[])
{
   for( int i = 1; i < argc; i++ )
   {
       int a = atoi(argv[i]);
       action(a,a);
   }
   return 0;
}
