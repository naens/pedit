#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>

#include "utf8conv.h"

int exerr(char *msg)
{
  fprintf(stderr, "%s\n", msg);
  exit(1);
}

struct ctb {
  uint32_t key;
  uint32_t *value;
};

/* reads single \xXX character */
uint32_t read_trchar(FILE *trsl)
{
  int ch;
  if (feof(trsl))
    return 0;
  ch = fgetc(trsl);

  if (ch == 0 || ch == '\n' || ch == -1)
    return 0;
  else if (ch != '\\')
    return read_utf8_b1(trsl, ch);
  if (feof(trsl))
    exerr("read_trchar:unxepected end of file in trsl");

  ch=fgetc(trsl);
  if (ch != 'x')
    exerr("read_trchar:unexpecter character in trsl");
  if (feof(trsl))
    exerr("read_trchar:unxepected end of file in trsl");
  
  char line[256];
  int i = 0;
  ch = fgetc(trsl);
  while (ch >= '0' && ch <= '9'
        || ch >= 'a' && ch <= 'f'
        || ch >= 'A' && ch <= 'F') {
    line[i] = ch;
    i++;
    if (feof(trsl))
      break;
    ch = fgetc(trsl);
  }

  if (i == 0)
    exerr("read_trchar:unexpected end of unicode character in trsl");

  ungetc(ch, trsl);
  return (uint32_t) strtol(line, NULL, 16);
}

/* reads transliteration line */
int read_trline(FILE *trsl, uint32_t *key, uint32_t **value)
{
  uint32_t k = read_trchar(trsl);
  if (k == 0)
    return -1;
  
  int ch;
  if (feof(trsl))
    return -1;
  ch = fgetc(trsl);

  if (ch != '\t')
    exerr("read_trline:unexpected character in trsl");
  if (feof(trsl))
    exerr("read_trline:unxepected end of file in trsl");

  uint32_t c = read_trchar(trsl);
  if (c == 0)
    exerr("read_trline:unexpected no value character");
  int max=255;
  uint32_t v[max+1];
  v[0]=c;
  int i = 0;
  do
  {
    i++;
    v[i] = read_trchar(trsl);
  } while (v[i] != 0);
  *value = malloc(sizeof(uint32_t) * (i + 1));
  int j = 0;
  while (j <= i)
  {
    (*value)[j]=v[j];
    j++;
  }
  *key = k;
  return 0;
}


int utf8skipbom(FILE *f)
{
  int byte = fgetc(f);
  if (byte == -1)
    return -1;
  if (byte != 0xef)
  {
    ungetc(byte, f);
    return 0;
  }
  byte = fgetc(f);
  if (byte == -1 || byte != 0xbb)
    return -1;
  byte = fgetc(f);
  if (byte == -1 || byte != 0xbf)
    return -1;
  return 0;
}

/* transliterates f1 to f2 using f3 */
int main(int argc, char **argv)
{
  if (argc != 4)
    exerr("usage: trslt from to translit");

  char *f1 = argv[1];
  char *f2 = argv[2];
  char *f3 = argv[3];

  /* open files */
  if (access(f2, F_OK) != -1)
    exerr("to file exists");

  FILE* from = fopen(f1, "r");
  if (from == NULL || utf8skipbom(from) != 0)
    exerr("could not open from file");

  FILE* to = fopen(f2, "w");
  if (to == NULL)
    exerr("could not open to file");

  FILE* trsl = fopen(f3, "r");
  if (trsl == NULL || utf8skipbom(trsl) != 0)
    exerr("could not open trsl file");

  /* get number of lines in trsl file */
  int lines=0;
  int ch;
  while ((ch = fgetc(trsl)) != EOF)
    if (ch == '\n') lines++;
  rewind(trsl);
  if (utf8skipbom(trsl) != 0)
    exerr("could not skip bom in trsl");

  printf("Total number of lines in trsl=%d\n", lines);    
  
  /* transliteration format: \xXXX\t(\xXXX)* */
  struct ctb ctb[lines];
  int i = 0;
  for (;;)
  {
    uint32_t key;
    uint32_t *value;
    if (read_trline(trsl, &key, &value) == -1)
      break;
    ctb[i].key = key;
    ctb[i].value = value;
    printf("Key=%x Value={ ", key);
    int k = 0;
    while (value[k] != 0)
      printf("%x ", value[k++]);
    printf("}\n");
    i++;
  }  

  fclose(from);
  fclose(to);
  fclose(trsl);
  int j = 0;
  while (j < lines)
  {
//    free(ctb[j].value);
    j++;
  }
}
