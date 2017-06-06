#include <stdio.h>
#include <stdlib.h>
#include <regex.h>        

/* usage:
 * arguments: <file> <header file name> <constant name>
 */

int main(int argc, char **argv)
{
  if (argc != 4)
    {
      fprintf(stderr, "Bad arguments.\n"
	      "Usage:  <file> <header file name> <constant name>\n");
      exit(1);
    }

  char *file_name = argv[1];
  char *header_file_name = argv[2];
  char *constant_name = argv[3];

  FILE *fi = fopen(file_name, "r");
  if (fi == NULL)
    {
      fprintf(stderr, "Error opening input file\n");
      exit(2);
    }

  FILE *fo = fopen(header_file_name, "w");
  if (fo == NULL)
    {
      fprintf(stderr, "Error opening output file\n");
      exit(3);
    }

  /* check variable name */
  regex_t regex;
  if (regcomp(&regex, "^[_a-zA-Z][\\._a-zA-Z0-9]*$", 0))
    {
      fprintf(stderr, "Could not compile regex\n");
      exit(4);
    }

  if (regexec(&regex, constant_name, 0, NULL, 0) != 0)
    {
      fprintf(stderr, "Bad constant name\n");
      exit(5);
    }


  fprintf(fo, "unsigned char %s[] = {\n  ", constant_name);

  int byte_num = 0;
  unsigned char b;
  for (;;)
    {
      if (fread(&b, 1, 1, fi) == 0)
	break;
      
      fprintf(fo, "0x%02x,", b);

      byte_num++;
      if (byte_num % 0x10 == 0)
	fprintf(fo, "\n  ");
    }

  fprintf(fo, "0x00\n};\nunsigned int %s_len = %d;\n", constant_name, byte_num + 1);
}

/* EXAMPLE

unsigned char a[] = {
  0x68, 0x65, 0x6c, 0x6c, 0x6f, 0x20, 0x77, 0x6f,
  0x72, 0x6c, 0x64, 0x0a, 0x00
};
unsigned int a_len = 13;

*/
