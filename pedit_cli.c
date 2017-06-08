#include <stdio.h>
#include <string.h>

#include <sqlite3.h>

#include "pedit_db.h"

int exerr(char *msg)
{
  fprintf(stderr, msg);
  fprintf(stderr, "\n");
  return 1;
}

int main(int argc, char **argv)
{
  if (argc != 2)
    return exerr("usage: <pedit_cli> <database file");

  char *dbfn = argv[1];
  sqlite3 *pDb;
  if (init_database(&pDb, dbfn) != 0)
    return exerr("could not open db");

  char *line = NULL;
  size_t size;
  char *args[0x100];
  while (getline(&line, &size, stdin) != -1)
  {
    char *cmd = strtok(line, " \t\n\r");
    int i = 0;
    char *arg = strtok(NULL, " \t\n\r");
    while (arg != NULL)
    {
      args[i] = arg;
      arg = strtok(NULL, " \t\n\t");
      i++;
    }
    int argsn = i;
    if (strcmp(cmd, "exit") == 0)
      break;
    printf("cmd=%s ", cmd);
    for (int a = 0; a < argsn; a++) printf("arg[%d]=%s ", a, args[a]);
    printf("\n");
    /* operations: */
    /* show-text: show text after a specified node */
    /* show-chars: display characters of a ti, thier w, wps and positions */
    /* split-wp: split within a text item, make new word parts */
    /* merge-wp: make several wp of one ti a single wp */
    /* combine-wp: make several wp belong to a single word */
    /* divide-word: make each wp of a word belong to its own word */
  }

  if (close_database(pDb) != 0)
    return exerr("could not close db");
  return 0;
}
