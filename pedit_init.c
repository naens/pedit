#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>

#include <sqlite3.h>

#include "pedit_db.h"

int exerr(char *msg)
{
  fprintf(stderr, "%s\n", msg);
  return -1;
}

void splitline(char *line, char **pre, char **text, char **post)
{
  int i = 0;
  *pre = line;
  while (line[i] && line[i] != '\t')
    i++;
  line[i] = 0;
  *text = &line[i+1];
  i++;
  while (line[i] && line[i] != '\t')
    i++;
  line[i] = 0;
  *post = &line[i+1];
  i++;
  while (line[i] && line[i] != '\n')
    i++;
  line[i] = 0;
}

char *findexec(char *name)
{
  char *tmp_path = getenv("PATH");
  if (tmp_path == NULL)
    exit(exerr("could not get PATH"));
  char path[0x1000];
  strncpy(path, tmp_path, sizeof path);
  char *token = strtok(path, ":");
  char *result = malloc(0x1000);
  result[0] = 0;
  while (token)
  {
    sprintf(result, "%s/%s", token, name);
    if (access(result, X_OK) == 0)
    {
      return result;
    }
    token = strtok(NULL, ":");
  }
  return 0;
}

int64_t create_tv(sqlite3 *pDb,
  char *language, char *text_name, char *tv_name, int64_t *text_id)
{
  /* get/create language */
  int found;
  int64_t lang_id;
  if (language_by_name(pDb, language, &found, &lang_id) != 0)
    return -1;

  if (!found && language_create(pDb, language, &lang_id) != 0)
    exit(exerr("could not create language"));
  fprintf(stderr, "create/find language %" PRId64 "\n", lang_id);

  /* create new text */
  if (text_create(pDb, lang_id, text_name, text_id) != 0)
    exit(exerr("could not create text"));
  fprintf(stderr, "create text %" PRId64 "\n", *text_id);

  /* create new text version */
  int64_t tv_id;
  if (tv_create(pDb, *text_id, tv_name, &tv_id) != 0)
    exit(exerr("could not create text version"));
  fprintf(stderr, "create text version %" PRId64 "\n", tv_id);
   
  return tv_id;
}

int64_t create_node(sqlite3 *pDb, int64_t text_id)
{
  /* get last node, connect if not the only one */
  int found;
  int64_t last_node_id;
  if (tn_get_last(pDb, text_id, &found, &last_node_id) != 0)
    exit(exerr("could not get last node"));

  /* create new node */
  int64_t node_id;
  if (tn_create(pDb, text_id, &node_id) != 0)
    exit(exerr("could not create node"));

  if (found && tn_connect(pDb, last_node_id, node_id) != 0)
    exit(exerr("could not connect nodes"));

  return node_id;  
}

void append_word(sqlite3 *pDb, int64_t text_id, int64_t tv_id,
       char* pre, char *item, char *post)
{
    /* create new text node */
    int64_t tn_id = create_node(pDb, text_id);

    /* create new text item */
    int64_t ti_id;
    if (ti_create(pDb, tn_id, &ti_id) != 0)
      exit(exerr("could not create text item"));

    if (ti_addtv(pDb, ti_id, tv_id) != 0)
      exit(exerr("could not add tv to ti"));
    
    /* create new text cell */
    if (tc_set(pDb, tn_id, tv_id, pre, post) != 0)
      exit(exerr("could not set text cell pre/post"));

    /* create new word */
    int64_t w_id;
    if (word_create(pDb, &w_id) != 0)
      exit(exerr("could not create word"));

    /* create new word part */
    int64_t wp_id;
    if (wp_create(pDb, ti_id, w_id, 1, item, &wp_id) != 0)
      exit(exerr("could not create word part"));
}

/* Imports a text file
 * arguments: <dbfile> <textfile> <sepfile> <lang>
 */
int main(int argc, char **argv)
{
  if (argc != 5)
    return exerr("arguments: <dbfile> <textfile> <sepsfile> <language>");

  /* TODO: organize arguments to have options with '-' or '--' */

  char *dfn = argv[1];
  char *tfn= argv[2];
  char *sfn = argv[3];
  char *language = argv[4];

  /* open files */
  FILE *tfd = fopen(tfn, "r");
  if (tfd == NULL)
    return exerr("could not open text file");
  FILE* sfd = fopen(sfn, "r");
  if (sfd == NULL)
    return exerr("could not open separators file");

  /* open database */
  sqlite3 *pDb;
  if (init_database(&pDb, dfn) != 0)
    return exerr("could not open db");

  /* add language, text and text version to the database */
  int64_t text_id;
  int64_t tv_id = create_tv(pDb, language, tfn, tfn, &text_id);

  /* read file */
  char path[0x1005];
  char cmd[0x1000];
  char *wdsep = findexec("wdsep");
  if (wdsep == 0)
    exerr("could not find wdsep in path");
  else
  {
    sprintf(cmd, "%s %s < %s", wdsep, sfn, tfn);
    free(wdsep);
  }
  FILE *fp = popen(cmd, "r");
  if (fp == NULL)
    exerr("error executing popen" );

  char *pre;
  char *item;
  char *post;
  while (fgets(path, sizeof(path)-1, fp) != NULL) {
    splitline(path, &pre, &item, &post);
    append_word(pDb, text_id, tv_id, pre, item, post);
  }
  pclose(fp);

  /* close files */
  fclose(tfd);
  fclose(sfd);

  /* close db */
  if (close_database(pDb) != 0)
    return exerr("could not exit db");

  /* exit */
  return 0;
}
