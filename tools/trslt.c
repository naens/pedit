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

int main(int argc, char **argv)
{
  FILE* from = stdin;
  FILE* to = stdout;
  FILE* trsl = argc == 2 ? fopen(argv[1], "r") : 0;
  if (trsl == NULL || utf8skipbom(trsl) != 0)
    trsl = 0;

  int lines = 0;
  struct ctb *ctbs = 0;
  if (trsl != 0)
  {
    /* get number of lines in trsl file */
    int ch;
    while ((ch = fgetc(trsl)) != EOF)
      if (ch == '\n') lines++;
    rewind(trsl);
    if (utf8skipbom(trsl) != 0)
      exerr("could not skip bom in trsl");

    /* transliteration format: \xXXX\t(\xXXX)* */
    ctbs = malloc(sizeof(struct ctb) * lines);
    int i = 0;
    for (;;)
    {
      uint32_t key;
      uint32_t *value;
      if (read_trline(trsl, &key, &value) == -1)
        break;
      ctbs[i].key = key;
      ctbs[i].value = value;
      i++;
    }
  }

  /* transliterate the text using ctb table */
  while (!feof(from))
  {
    uint32_t c = read_utf8(from);
    if (c == -1)
      break;

    /* find in ctb table */
    if (ctbs != 0)
    {
      struct ctb *ctb = find_ctb(ctbs, lines, c);

      /* if in table, write table value */
      if (ctb != 0)
        for (uint32_t *pchr = ctb->value; *pchr; pchr++)
          write_utf8(to, *pchr);
      else  /* if not in table, write original value */
        write_utf8(to, c);
    }
    else
      write_utf8(to, c);
  }

  /* close files and free variables */
  if (trsl != 0)
  {
    fclose(trsl);
    int j = 0;
    while (j < lines)
    {
      free(ctbs[j].value);
      j++;
    }
    free(ctbs);
  }
}
