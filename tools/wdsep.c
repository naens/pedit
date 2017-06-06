#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>

#include "utf8conv.h"

uint32_t defbwcs[3] = { '\n', ' ', '\t' };
int defbwn = sizeof(defbwcs) / sizeof(defbwcs[0]);

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
    tmpbuf[tmpsz] = utf8chr;
    tmpsz++;
  }
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

int is_word_char(uint32_t ch,
            int bfn, uint32_t *bfcs, int afn, uint32_t *afcs,
            int arn, uint32_t *arcs, int bwn, uint32_t *bwcs)
{
  return (ch != EOF && !contains(bfn, bfcs, ch) && !contains(afn, afcs, ch)
          && !contains(arn, arcs, ch) && !contains(bwn, bwcs, ch));
}

int getprepos(int sz, int lbw, int fbf, int laf)
{
  if (lbw != -1)
    return lbw + 1;
  else if (fbf != -1)
    return fbf;
  else if (laf != -1)
    return laf + 1;
}

int is_space(char c)
{
  return c == ' ' || c == '\n' || c == '\r' || c == '\t';
}

int trimstr(char *str, int sz)
{
  int i = 0;
  while (str[i] && is_space(str[i]))
    i++;
  int j = sz - 1;
  while (is_space(str[j]) && j > i)
    j--;
  str[j+1] = 0;
  return i;
}

/* separates words based on before, after, between settings
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

  FILE *in_f = stdin;
  FILE *out_f = stdout;

  FILE *sep_f;
  if (argc == 2)
    sep_f = fopen(argv[1], "r");
  else
    sep_f = 0;
  if (sep_f == NULL)
    sep_f = 0;

  int bfn;
  uint32_t *bfcs;
  int afn;
  uint32_t *afcs;
  int arn;
  uint32_t *arcs;
  int bwn;
  uint32_t *bwcs;
  if (sep_f != 0)
  {
    if (utf8skipbom(sep_f) == -1)
      return exerr("wdsep main: could not skip bom");
  /* read separtors from separator file */
    readsepline(sep_f, &bfn, &bfcs);
    readsepline(sep_f, &afn, &afcs);
    readsepline(sep_f, &arn, &arcs);  
    readsepline(sep_f, &bwn, &bwcs);
  }
  else
  {
    bfn = 0;
    bfcs = 0;
    afn = 0;
    afcs = 0;
    arn = 0;
    arcs = 0;
    bwn = defbwn;
    bwcs = defbwcs;
  }
  /* read input file separating words to output file
   * output file format: <pre><tab><word><tab><post>
   */
  if (utf8skipbom(in_f) == -1)
    return exerr("wdsep main: could not skip bom");

// no need to write bom
//  if (utf8writebom(out_f) == -1)
//    return exerr("wdsep main: could not write bom");
  
  /* first: consider all characters before first word as pre */
  int ch = getc(in_f);
  while (ch != EOF && !is_word_char(ch, bfn, bfcs, afn, afcs, arn, arcs, bwn, bwcs))
  {
    fputc(ch, out_f);
    ch = getc(in_f);
  }
  fputc('\t', out_f);

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
    fputc('\t', out_f);

    /* end if eof after previous word */
    if (feof(in_f))
    {
      fputc('\n', out_f);
      break;
    }

    /* read non-word characters to buffer */
    int i = 0;
    char buf[0x1000];
    int lbw = -1;     /* last-between */
    int fbf = -1;     /* first-before */
    int laf = -1;     /* last-after */
    while (ch != EOF && !is_word_char(ch, bfn, bfcs, afn, afcs, arn, arcs, bwn, bwcs))
    {
      if (contains(bwn, bwcs, ch))
        lbw = i;
      if (fbf == -1 && contains(bfn, bfcs, ch))
        fbf = i;
      if (contains(afn, afcs, ch))
        laf = i;
      buf[i] = ch;
      i++;
      ch = getc(in_f);
    }

    if (feof(in_f))
    {
      int j = trimstr(buf, i);
//      fprintf(out_f, "%s", &buf[j]);
//      printf("exiting: <%s>\n", &buf[j]);
//      for (; j < i; j++)
      while (buf[j] && j < i)
      {
        fputc(buf[j], out_f);
        j++;
      }
      fputc('\n', out_f);
      break;
    }

    /* post */
    int sep = getprepos(i, lbw, fbf, laf);
    int j = trimstr(buf, sep);
    while (buf[j] && j < sep)
    {
      fputc(buf[j], out_f);
      j++;
    }
    fputc('\n', out_f);

    /* pre */
    j = trimstr(&buf[sep], i - sep) + sep;
    while (buf[j] && j < i)
    {
      fputc(buf[j], out_f);
      j++;
    }
    fputc('\t', out_f);
  }

  /* free and close */
  if (sep_f != 0)
  {
    free(bfcs);
    free(afcs);
    free(arcs);
    free(bwcs);
    fclose(sep_f);
  }
}
