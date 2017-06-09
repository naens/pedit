#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <stdlib.h>
#include <inttypes.h>

#include <sqlite3.h>

#include "pedit_db.h"

#define SHOW_TEXT_LIM 12

int exerr(char *msg)
{
  fprintf(stderr, "%s\n", msg);
  return 1;
}

/* show-text: show text after a specified node */
int64_t last_node;
int is_last_node_valid = 0;
void show_text(sqlite3* pDb, int64_t text_id, int sz, char *args[])
{
  int64_t node_id;
  int found;
  if (sz == 0 && !is_last_node_valid)
  {
     if (tn_get_first(pDb, text_id, &found, &node_id) != 0)
       exit(exerr("could not get first node"));
  }
  else if (sz == 1 || is_last_node_valid)
  {
    node_id = (sz == 1) ? atoll(args[0]) : last_node;
    if (tn_exists(pDb, node_id, &found) != 0)
      exit(exerr("could not find whether node exists"));
  }
  else
  {
    printf("args: node_id\n");
    return;
  }
  if (!found)
  {
    is_last_node_valid = 0;
    printf("no node\n");
    return;
  }
  
    
  /* display rows after node node_id*/
  int i = 0;
  while (found && i < SHOW_TEXT_LIM)
  {
    /* find string */
    /* find pre, pos */
    /* find word_part */
    /* find word */

    printf("node %" PRId64 "\n", node_id);
    last_node = node_id;
    is_last_node_valid = 1;

    /* find next */
    if (tn_get_next(pDb, node_id, &found, &node_id) != 0)
      exit(exerr("could not find next node"));
    i++;
  }
}

/* show-chars: display characters of a ti, thier w, wps and positions */
void show_chars(sqlite3* pDb, int sz, char *args[])
{
  printf("show-chars, args: ");
  for (int i = 0; i < sz; i++)
    printf("[%d]:%s ", i, args[i]);
  printf("\n");
}

/* split-wp: split within a text item, make new word parts */
void split_wp(sqlite3* pDb, int sz, char *args[])
{
  printf("split-wp, args: ");
  for (int i = 0; i < sz; i++)
    printf("[%d]:%s ", i, args[i]);
  printf("\n");
}

/* merge-wp: make several wp of one ti a single wp */
void merge_wp(sqlite3* pDb, int sz, char *args[])
{
  printf("merge-wp, args: ");
  for (int i = 0; i < sz; i++)
    printf("[%d]:%s ", i, args[i]);
  printf("\n");
}

/* combine-wp: make several wp belong to a single word */
void combine_wp(sqlite3* pDb, int sz, char *args[])
{
  printf("combine-wp, args: ");
  for (int i = 0; i < sz; i++)
    printf("[%d]:%s ", i, args[i]);
  printf("\n");
}

/* divide-word: make each wp of a word belong to its own word */
void divide_word(sqlite3* pDb, int sz, char *args[])
{
  printf("divide-word, args: ");
  for (int i = 0; i < sz; i++)
    printf("[%d]:%s ", i, args[i]);
  printf("\n");
}

void print_help()
{
  printf("pedit_cli commands:\n");
  printf("show-text: show text after a specified node\n");
  printf("show-chars: display characters of a ti, thier w, wps and positions\n");
  printf("split-wp: split within a text item, make new word parts\n");
  printf("merge-wp: make several wp of one ti a single wp\n");
  printf("combine-wp: make several wp belong to a single word\n");
  printf("divide-word: make each wp of a word belong to its own word\n");
  printf("exit: exit pedit_cli\n");
  printf("\n");
}

void kbstop()
{
  print_help();
}

int main(int argc, char **argv)
{
  signal(SIGINT, kbstop);
  signal(SIGTERM, kbstop);
  signal(SIGQUIT, kbstop);

  if (argc != 3)
    return exerr("usage: <pedit_cli> <database file> <text name>");

  char *dbfn = argv[1];
  sqlite3 *pDb;
  if (init_database(&pDb, dbfn) != 0)
    return exerr("could not open db");

  char *text_name = argv[2];
  int64_t text_id;
  int found;
  if (text_by_name(pDb, text_name, &found, &text_id) != 0)
    return exerr("could not find text by name");
  if (!found)
    return exerr("could not find text by name");

  char *line = NULL;
  size_t size;
  char *args[0x100];
  int argsn;
  while (getline(&line, &size, stdin) != -1)
  {
    char *cmd = strtok(line, " \t\n\r");
    if (cmd != NULL)
    {
      int i = 0;
      char *arg = strtok(NULL, " \t\n\r");
      while (arg != NULL)
      {
        args[i] = arg;
        arg = strtok(NULL, " \t\n\t");
        i++;
      }
      argsn = i;
    }
    else
      argsn = 0;
    if (cmd == NULL || strcmp(cmd, "show-text") == 0)
      show_text(pDb, text_id, argsn, args);
    else if (strcmp(cmd, "show-chars") == 0)
      show_chars(pDb, argsn, args);
    else if (strcmp(cmd, "split-wp") == 0)
      split_wp(pDb, argsn, args);
    else if (strcmp(cmd, "merge-wp") == 0)
      merge_wp(pDb, argsn, args);
    else if (strcmp(cmd, "combine-wp") == 0)
      combine_wp(pDb, argsn, args);
    else if (strcmp(cmd, "divide-word") == 0)
      divide_word(pDb, argsn, args);
    else if (strcmp(cmd, "exit") == 0)
      break;
    else
      print_help();
  }

  if (close_database(pDb) != 0)
    return exerr("could not close db");
  return 0;
}
