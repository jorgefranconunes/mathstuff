#include <stdio.h>

#include <ardevents/helloutils.h>





int main (int argc, char **argv) {

    char *packageName = getPackageName();

    printf("Hello, world!\n");
    printf("This message brought to you by: %s\n", packageName);

    return 0;
}

