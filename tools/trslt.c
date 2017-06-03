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

struct ctb *find_ctb(struct ctb *ctbs, int sz, uint32_t key)
{
  struct ctb *res = 0;
  for (int i = 0; res == 0 && i < sz; i++)
    if (ctbs[i].key == key)
      res = &ctbs[i];
  return res;
}

/* reads transliteration line */
int read_trline(FILE *trsl, uint32_t *key, uint32_t **value)
{
  uint32_t k = read_char(trsl);
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

  uint32_t c = read_char(trsl);
  if (c == 0)
    exerr("read_trline:unexpected no value character");
  int max=255;
  uint32_t v[max+1];
  v[0]=c;
  int i = 0;
  do
  {
    i++;
    v[i] = read_char(trsl);
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

/* transliterates f1 to f2 using f3 */
int main(int argc, char **argv)
{
  if (argc != 4)
    exerr("usage: trslt from to translit");

  char *f1 = argv[1];
  char *f2 = argv[2];
  char *f3 = argv[3];

  /* open files */
//  if (access(f2, F_OK) != -1)
//    exerr("to file exists");

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
  struct ctb ctbs[lines];
  int i = 0;
  for (;;)
  {
    uint32_t key;
    uint32_t *value;
    if (read_trline(trsl, &key, &value) == -1)
      break;
    ctbs[i].key = key;
    ctbs[i].value = value;
    printf("Key=%x Value={ ", key);
    int k = 0;
    while (value[k] != 0)
      printf("%x ", value[k++]);
    printf("}\n");
    i++;
  }

  /* transliterate the text using ctb table */
//  FILE *allchars = fopen("all.txt", "w");
  if (utf8writebom(to) == -1)
    exerr("trsl main: error writing bom to to file");
  while (!feof(from))
  {
    uint32_t c = read_utf8(from);
    if (c == -1)
      break;

    /* find in ctb table */
    struct ctb *ctb = find_ctb(ctbs, lines, c);
//    write_utf8(allchars, c);
//    write_utf8(allchars, '\n');

    /* if in table, write table value */
    if (ctb != 0)
      for (uint32_t *pchr = ctb->value; *pchr; pchr++)
        write_utf8(to, *pchr);
    else  /* if not in table, write original value */
      write_utf8(to, c);
  }

  /* close files and free variables */
  fclose(from);
  fclose(to);
  fclose(trsl);
  int j = 0;
  while (j < lines)
  {
    free(ctbs[j].value);
    j++;
  }
}
