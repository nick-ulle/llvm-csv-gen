
#include <stdio.h>

int main(int argc, char **argv)
{
  FILE *foo = fopen("foo.txt", "r");

  char buffer[64];
  fgets(buffer, 10, foo);

  printf("%s\n", buffer);

  fclose(foo);
}
