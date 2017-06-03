#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>

#include "utf8conv.h"

int exerr(char *msg)
{
  fprintf(stderr, msg);
  fprintf(stderr, "\n");
  return 1;
}

/* reads one line of separators and stores them in *pbuf of size *sz */
void readsepline(FILE *fd, int *sz, uint32_t **pbuf)
{
  int tmpsz = 0;
  uint32_t tmpbuf[0x1000];
  for (;;) {
    uint32_t utf8chr = read_char(fd);
    if (utf8chr == -1)
       exit(exerr("readsepline: error reading utf8-character"));
    if (utf8chr == 0)
      break;
    printf("[%02x=%c]", utf8chr, utf8chr);
    tmpbuf[tmpsz] = utf8chr;
    tmpsz++;
  }
  printf(".\n");
  *sz = tmpsz;
  *pbuf = malloc(tmpsz * sizeof(uint32_t));
  for (int i = 0; i < tmpsz; i++)
    (*pbuf)[i] = tmpbuf[i];
}

int contains(int sz, uint32_t *str, uint32_t ch)
{
  for (int i = 0; i < sz; i++)
    if (str[i] == ch)
      return 1;
  return 0;
}

/* TODO: check: end of file is not a word character!!! */
int is_word_char(uint32_t ch,
            int bfn, uint32_t *bfcs, int afn, uint32_t *afcs,
            int arn, uint32_t *arcs, int bwn, uint32_t *bwcs)
{
  return (ch != EOF && !contains(bfn, bfcs, ch) && !contains(afn, afcs, ch)
          && !contains(arn, arcs, ch) && !contains(bwn, bwcs, ch));
}


/* separates words based on before, after, between settings
 * usage wdsep <in file> <out file> <separators file>
 * separators file should contain 5 lines:
    * before word characters (opening parentheses, brackets).
    * after word characters (closing parens, barckets, punctuation like :.!)
    * around word characters (can be before and after, like ' or ")
    * between word characters
    * empty line
   any of the lines can be empty 
 */
int main(int argc, char **argv)
{
  if (argc < 3)
    return exerr("Usage: wdsep <in-file> <out-file> <separators-file>");
  char *in_fn = argv[1];
  char *out_fn = argv[2];
  char *sep_fn = argv[3];

  /* open all files */
  FILE *in_f = fopen(in_fn, "r");
  if (in_f == NULL)
    return exerr("could not open in file");

//  if (access(out_fn, F_OK) != -1)
//    return exerr("out file exists");
  FILE *out_f = fopen(out_fn, "w");
  if (out_f == NULL)
    return exerr("could not open out file");

  FILE *sep_f = fopen(sep_fn, "r");
  if (sep_f == NULL)
    return exerr("could not open sep file");

  /* read separtors from separator file */
  if (utf8skipbom(sep_f) == -1)
    return exerr("wdsep main: could not skip bom");
  int bfn;
  uint32_t *bfcs;
  readsepline(sep_f, &bfn, &bfcs);
  
  int afn;
  uint32_t *afcs;
  readsepline(sep_f, &afn, &afcs);
  
  int arn;
  uint32_t *arcs;
  readsepline(sep_f, &arn, &arcs);  

  int bwn;
  uint32_t *bwcs;
  readsepline(sep_f, &bwn, &bwcs);

  /* read input file separating words to output file
   * output file format: <pre><tab><word><tab><post>
   */
  if (utf8skipbom(in_f) == -1)
    return exerr("wdsep main: could not skip bom");

  if (utf8writebom(out_f) == -1)
    return exerr("wdsep main: could not write bom");
  
  /* first: consider all characters before first word as pre */
  int ch = getc(in_f);
  while (!is_word_char(ch, bfn, bfcs, afn, afcs, arn, arcs, bwn, bwcs))
  {
    fputc(ch, out_f);
    ch = getc(in_f);
  }

  /* main loop: from the beginning of the word (ch = first char):
     * read word to end (writing to output)
     * read everything up to next word
     * decide where the next word begins
     * write to output post of the previous word and the pre of the next.
     * after loop: last word with pre, post and newline
     */
  while (!feof(in_f))
  {
    /* read while word characters */
    while (is_word_char(ch, bfn, bfcs, afn, afcs, arn, arcs, bwn, bwcs))
    {
      fputc(ch, out_f);
      ch = getc(in_f);
    }

    /* end if eof after previous word */
    if (feof(in_f))
    {
      fputc('\n', out_f);
      break;
    }

    /* read non-word characters to buffer */
    /* TODO: find last-between, first-before, last-after */
    int i = 0;
    uint32_t buf[0x1000];
    while (!is_word_char(ch, bfn, bfcs, afn, afcs, arn, arcs, bwn, bwcs))
    {
      buf[i] = ch;
      ch = getc(in_f);
      i++;
    }
    
    /* write to post and pre to file */
    /* TODO */
    /* if EOF: => everything after the end of the word is post */
  }

  /* free and close */
  free(bfcs);
  free(afcs);
  free(bwcs);
  fclose(in_f);
  fclose(out_f);
  fclose(sep_f);
}
