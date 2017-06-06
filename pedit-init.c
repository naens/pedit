#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

#include <sqlite3.h>

#include "create_db.h"

int exerr(char *msg)
{
  fprintf(stderr, msg);
  fputc('\n', stderr);
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

/* Imports a text file
 * arguments: <dbfile> <textfile> <sepfile>
 */
int main(int argc, char **argv)
{
  if (argc != 4)
    return exerr("arguments: <dbfile> <textfile> <sepsfile>");

  char *dfn = argv[1];
  char *tfn= argv[2];
  char *sfn = argv[3];

  /* open files */
  FILE *tfd = fopen(tfn, "r");
  if (tfd == NULL)
    return exerr("could not open text file");
  FILE* sfd = fopen(sfn, "r");
  if (sfd == NULL)
    return exerr("could not open separators file");

  /* open database */
  sqlite3 *pDb;
  int new_db = (access(dfn, 0) != 0);
  if (sqlite3_open(dfn, &pDb) != SQLITE_OK)
    return exerr("could not open db");
  
  /* execute database creation script if no db file found */
  if (new_db && sqlite3_exec(pDb, create_db, NULL, NULL, NULL) != SQLITE_OK)
    exerr("could not exectute create db script");

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
  char *text;
  char *post;
  while (fgets(path, sizeof(path)-1, fp) != NULL) {
    splitline(path, &pre, &text, &post);
    printf("<%s|%s|%s>\n", pre, text, post);
  }
  pclose(fp);

  /* close files */
  fclose(tfd);
  fclose(sfd);

  /* close db */
  if (sqlite3_close(pDb) != SQLITE_OK)
    return exerr("could not exit db");

  /* exit */
  return 0;
}
