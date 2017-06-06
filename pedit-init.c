#include <stdio.h>
#include <unistd.h>

#include <sqlite3.h>

#include "create_db.h"

int exerr(char *msg)
{
  fprintf(stderr, msg);
  fputc('\n', stderr);
  return -1;
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

  /* read separators */

  /* read file */

  /* close files */
  fclose(tfd);
  fclose(sfd);

  /* close db */
  if (sqlite3_close(pDb) != SQLITE_OK)
    return exerr("could not exit db");

  /* exit */
  return 0;
}
