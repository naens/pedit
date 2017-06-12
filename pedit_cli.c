#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <stdlib.h>
#include <inttypes.h>

#include <sqlite3.h>

#include "pedit_db.h"
#include "tools/utf8conv.h"

#define SHOW_TEXT_LIM 12

int exerr(char *msg)
{
  fprintf(stderr, "%s\n", msg);
  return 1;
}


void get_ti_str(sqlite3 *pDb, int64_t node_id, int64_t tv_id, char *ti_str)
{
  /* get ti id */
  int ti_found;
  int64_t ti_id;
  if (ti_get(pDb, node_id, tv_id, &ti_found, &ti_id) != 0)
    exit(exerr("could not get ti"));
  if (!ti_found)
  {
    sprintf(ti_str, "ti[NOPE]");
    return;
  }

  /* get word parts */
  int wps_found;
  int wps_sz;
  int64_t *wps;
  if (wp_get_by_ti(pDb, ti_id, &wps_found, &wps_sz, &wps) != 0)
    exit(exerr("get_ti_str: could not get word parts"));
  if (!wps_found)
  {
    sprintf(ti_str, "ti%" PRId64 ":<NO_WPS>");
    return;
  }

  /* ti string format "ti<ti_id>:<<wp strings>>"
  /* wp string format "w<w_id>:wp<wp_id>:<string>" */
  int x = sprintf(ti_str, "ti%" PRId64 ":<", ti_id);
  for (int i = 0; i < wps_sz; i++)
  {
    if (i > 0)
      x += sprintf(&ti_str[x], "|");

    /* get word part word id */
    int w_found;
    int64_t w_id;
    if (wp_get_word(pDb, wps[i], &w_found, &w_id) != 0)
      exit(exerr("could not get word id of a word part"));
    if (w_found)
      x += sprintf(&ti_str[x], "w%" PRId64 ":", w_id);
    else
      x += sprintf(&ti_str[x], "w[NOPE]:");
    x += sprintf(&ti_str[x], "wp%" PRId64 ":", wps[i]);

    /* get word part text */
    int wptext_found;
    char *wptext;
    if (wp_get_text(pDb, wps[i], &wptext_found, &wptext) != 0)
      exit(exerr("could not get word part text"));
    if (wptext_found)
    {
      x += sprintf(&ti_str[x], "%s", wptext);
      free(wptext);
    }
    else
      x += sprintf(&ti_str[x], "[NO_STRING]");
  }
  sprintf(&ti_str[x], ">");
  free(wps);
}


/* show-text: show text after a specified node */
int64_t last_node;
int is_last_node_valid = 0;
void show_text(sqlite3* pDb, int64_t text_id, int64_t tv_id,
                int sz, char *args[])
{
  int64_t tn_id;
  int found;
  if (sz == 0 && !is_last_node_valid)
  {
     if (tn_get_first(pDb, text_id, &found, &tn_id) != 0)
       exit(exerr("could not get first node"));
  }
  else if (sz == 1 || is_last_node_valid)
  {
    if (is_last_node_valid && sz == 0)
      tn_id = last_node;
    else
    {
      char *node_idstr = args[0];
      if (node_idstr[0] == 'n')
        node_idstr = &node_idstr[1];
      tn_id = atoll(node_idstr);
    }

    if (tn_exists(pDb, tn_id, &found) != 0)
      exit(exerr("could not find whether node exists"));
  }
  else
  {
    printf("args: tn_id\n");
    return;
  }
  if (!found)
  {
    is_last_node_valid = 0;
    printf("no node\n");
    return;
  }
  
    
  /* display rows after node tn_id.
   * Format n<tn_id><T><pre><T><ti><T><post> 
   *   <ti>: (w<w_id>:wp<wp_id>:<string>) separated by '|' */
  int i = 0;
  char ti_str[0x1000];
  while (found && i < SHOW_TEXT_LIM)
  {
    get_ti_str(pDb, tn_id, tv_id, ti_str);
    char *pre;
    char *post;
    int tc_found;
    if (tc_get(pDb, tn_id, tv_id, &tc_found, &pre, &post) != 0)
      exit(exerr("could not get tc"));
    printf("n%" PRId64 "\t<%s>\t%*s\t\t<%s>\n", tn_id, pre,
                                                      32, ti_str, post);

//    printf("%" PRId64 "%s%s%s\n", tn_id, pre, ti_str, post);
    free(pre);
    free(post);
    
    /* find next */
    if (tn_get_next(pDb, tn_id, &found, &tn_id) != 0)
      exit(exerr("could not find next node"));
    i++;

    last_node = tn_id;
    is_last_node_valid = found;
  }
}

/* show-chars: display characters of a ti, thier w, wps and positions */
void show_chars(sqlite3* pDb, int64_t tv_id, int sz, char *args[])
{
  if (sz != 1)
  {
    printf("show-chars: enter a node id\n");
    return;
  }
  char *node_idstr = args[0];
  if (node_idstr[0] == 'n')
    node_idstr = &node_idstr[1];

  /* get text node */
  int tn_found;
  int64_t tn_id = atoll(node_idstr);
  if (tn_exists(pDb, tn_id, &tn_found) != 0)
    exit(exerr("show_chars: could not find whether node exists"));
  if (!tn_found)
  {
    printf("show-chars: wrong node id\n");
    return;
  }
  else printf("show-chars: node id =%" PRId64 "\n", tn_id);

  /* get text item */
  int ti_found;
  int64_t ti_id;
  if (ti_get(pDb, tn_id, tv_id, &ti_found, &ti_id) != 0)
    exit(exerr("could not get ti"));
  if (!ti_found)
  {
    printf("text item not found\n");
    return;
  }

  /* get word parts */
  int wps_found;
  int wps_sz;
  int64_t *wps;
  if (wp_get_by_ti(pDb, ti_id, &wps_found, &wps_sz, &wps) != 0)
    exit(exerr("could not get word parts"));
  if (!wps_found)
  {
    printf("no word parts found at node\n");
    return;
  }

  /* display chars.  Format: wp<wp_id>:<char pos><T><char> */
  for (int i = 0; i < wps_sz; i++)
  {
    /* get word part text */
    int wptext_found;
    char *wptext;
    if (wp_get_text(pDb, wps[i], &wptext_found, &wptext) != 0)
      exit(exerr("could not get word part text"));
    if (wptext_found)
    {
      char utf8buf[7];
      int j = 0;
      j += copyutf8char(utf8buf, &wptext[j]);
      int chnum = 1;
      while (utf8buf[0])
      {
        printf("wp%" PRId64 ":%d\t%s\n", wps[i], chnum, utf8buf);
        j += copyutf8char(utf8buf, &wptext[j]);
        chnum++;
      }
    }
    free(wptext);
  }
}

/* split-wp: split within a text item, make new word parts */
void split_wp(sqlite3* pDb, int sz, char *args[])
{
  /* args: <node_id> <wp_id> <char pos> => create new word part */
  if (sz != 3)
  {
    printf("split-wp <node> <wp> <pos>\n");
    return;
  }
  char *node_idstr = args[0];
  if (node_idstr[0] == 'n')
    node_idstr = &node_idstr[1];
  char *wp_idstr = args[1];
  if (wp_idstr[0] == 'w' && wp_idstr[1] == 'p')
    wp_idstr = &wp_idstr[2];
  int64_t tn_id = atoll(node_idstr);
  int64_t wp_id = atoll(wp_idstr);
  int pos = atoi(args[2]);

  /* check node exists */
  int node_found;
  if (tn_exists(pDb, tn_id, &node_found) != 0)
    exit(exerr("merge_wp: tn_exist problem"));
  if (!node_found)
  {
    printf("wrong node id");
    return;
  }

  /* get wp_text */
  int wptext_found;
  char *wptext;
  if (wp_get_text(pDb, wp_id, &wptext_found, &wptext) != 0)
    exit(exerr("split_wp: could not get wp text"));
  if (!wptext_found)
  {
    printf("no text found\n");
    return;
  }

  /* check pos: pos is [1..len], legal values are [2..len] */
  int utf8pos = getutf8pos(wptext, pos - 1);
  if (utf8pos == 0 || wptext[utf8pos] == 0)
  {
    printf("pos must be between 2 and length-1\n");
    return;
  }
  printf("split pos is %d (in string: %d)\n", pos, utf8pos);

  int wptext2len = strlen(&wptext[utf8pos]);
  char wptext2[wptext2len+1];
  memcpy(wptext2, &wptext[utf8pos], wptext2len + 1);
  wptext[utf8pos] = 0;
  int64_t wp_new_id;
  if (wp_split(pDb, wp_id, wptext, wptext2, &wp_new_id) != 0)
    exit(exerr("split-wp: could not split wp"));
  free(wptext);
}

/* merge-wp: make several wp of one ti a single wp */
void merge_wp(sqlite3* pDb, int64_t tv_id, int sz, char *args[])
{
  /* args: <node_id> => merge all word parts at node */
  if (sz != 1)
  {
    printf("merge-wp <node>\n");
    return;
  }
  char *node_idstr = args[0];
  if (node_idstr[0] == 'n')
    node_idstr = &node_idstr[1];
  int64_t tn_id = atoll(node_idstr);

  /* check node exists */
  int node_found;
  if (tn_exists(pDb, tn_id, &node_found) != 0)
    exit(exerr("merge_wp: tn_exist problem"));
  if (!node_found)
  {
    printf("wrong node id\n");
    return;
  }

  /* get text item */
  int ti_found;
  int64_t ti_id;
  if (ti_get(pDb, tn_id, tv_id, &ti_found, &ti_id) != 0)
    exit(exerr("merge_wp: could not get ti"));
  if (!ti_found)
  {
    printf("text item not found\n");
    return;
  }

  /* get word parts */
  int wps_found;
  int wps_sz;
  int64_t *wps;
  if (wp_get_by_ti(pDb, ti_id, &wps_found, &wps_sz, &wps) != 0)
    exit(exerr("merge_wp: could not get word parts"));
  if (!wps_found)
  {
    printf("no word parts found at node\n");
    return;
  }

  /* merge word parts into single string */
  int bufszstep = 0x1000;
  int bufsz = bufszstep;
  char *mergedbuf = malloc(bufsz);
  int lentot = 0;
  for (int i = 0; i < wps_sz; i++)
  {
    /* get word part text */
    int wptext_found;
    char *wptext;
    if (wp_get_text(pDb, wps[i], &wptext_found, &wptext) != 0)
      exit(exerr("merge_wp: could not get word part text"));
    if (wptext_found)
    {
      int wptext_len = strlen(wptext);
      if (lentot + wptext_len + 1 > bufsz)
      {
        int newbufsz = bufsz + wptext_len + bufszstep;
        char *newbuf = malloc(newbufsz);
        memcpy(newbuf, mergedbuf, bufsz);
        free(mergedbuf);
        mergedbuf = newbuf;
        bufsz = newbufsz;
      }
      memcpy(&mergedbuf[lentot], wptext, wptext_len);
      lentot += wptext_len;
    }
    free(wptext);

    /* get word part word id */
    int w_found;
    int64_t w_id;
    if (wp_get_word(pDb, wps[i], &w_found, &w_id) != 0)
      exit(exerr("merge_wp: could not get wp w_id"));
    if (!w_found)
      exit(exerr("merge_wp: error: wp without w"));

    /* delete word part */
    if (i > 0 && wp_delete(pDb, wps[i]) != 0)
      exit(exerr("merge_wp: could not delete word part"));

    /* if some word have no more wps, delete => update order! */
    int w_wps_found;
    int w_wps_sz;
    int64_t *w_wps;
    if(wp_get_by_w(pDb, w_id, &w_wps_found, &w_wps_sz, &w_wps) != 0)
      exit(exerr("merge_wp: could not get word wps"));
    if (w_wps_found && w_wps_sz == 0)
    {
      if (wp_delete(pDb, w_id) != 0)
        exit(exerr("merge_wp: could not delete word"));
    }
  }
  mergedbuf[lentot] = 0;
  
  /* modify wp text */
  if (wp_set_text(pDb, wps[0], mergedbuf) != 0)
    exit(exerr("merge_wp: could not set text"));

  free(mergedbuf);
  free(wps);
}

/* append-wp */
void append_wp(sqlite3* pDb, int sz, char *args[])
{
  /* args: <wp_id> <w_id> */
  if (sz != 2)
  {
    printf("append-wp <wp> <word>\n");
    return;
  }
  char *wp_idstr = args[0];
  if (wp_idstr[0] == 'w' && wp_idstr[1] == 'p')
    wp_idstr = &wp_idstr[2];
  char *w_idstr = args[1];
  if (w_idstr[0] == 'w')
    w_idstr = &w_idstr[1];
  int64_t w_id = atoll(w_idstr);
  int64_t wp_id = atoll(wp_idstr);

  printf("append_wp: w_id=%" PRId64 " wp_id=%" PRId64 "\n", w_id, wp_id);
  /* get current word of the word part */
  int w0_found;
  int64_t w0_id;
  if (wp_get_word(pDb, wp_id, &w0_found, &w0_id) != 0)
    exit(exerr("append_wp: could not get wp w_id"));
  if (!w0_found)
    exit(exerr("append_wp: error: wp without w"));

  /* nothing to do if the word id is already the right word id */
  if (w0_id == w_id)
  {
    printf("wp already in w");
    return;
  }
  
  /* update word of the word part */
  if (wp_set_word(pDb, wp_id, w_id) != 0)
    exit(exerr("append_wp: error setting wp word"));
  printf("append_wp: update word: w%" PRId64 " on wp%" PRId64 "\n", w_id, wp_id);

  /* get wps of the old word (w0) */
  int wp_count;
  if (word_wp_count(pDb, w0_id, &wp_count) != 0)
    exit(exerr("append_wp: error wp count"));

  printf("append_wp: wp_count=%d w0_id=%" PRId64 "\n", wp_count, w0_id);
  /* delete old word if empty */
  if (wp_count == 0 && word_delete(pDb, w0_id) != 0)
    exit(exerr("append_wp: could not delete empty word"));
}

/* separate-wp */
void separate_wp(sqlite3* pDb, int sz, char *args[])
{
  /* args: <wp_id> */
  if (sz != 1)
  {
    printf("separate-wp <wp>\n");
    return;
  }
  char *wp_idstr = args[0];
  if (wp_idstr[0] == 'w' && wp_idstr[1] == 'p')
    wp_idstr = &wp_idstr[2];
  int64_t wp_id = atoll(wp_idstr);

  /* get word part word */
  int w_found;
  int64_t w_id;
  if (wp_get_word(pDb, wp_id, &w_found, &w_id) != 0)
    exit(exerr("separate_wp: could not get wp w_id"));
  if (!w_found)
    exit(exerr("separate_wp: error: wp without w"));

  /* check the number of word parts in its word */
  int wp_count;
  if (word_wp_count(pDb, w_id, &wp_count) != 0)
    exit(exerr("separate_wp: error wp count"));

  /* nothing to do if it is the only word part in the word */
  if (wp_count == 1)
    return;
  
  /* create new word */
  int64_t wnew_id;
  if (word_create(pDb, &wnew_id) != 0)
    exit(exerr("separate_wp: error creating new word"));

  /* set wp word to new */
  if (wp_set_word(pDb, wp_id, wnew_id) != 0)
    exit(exerr("separate_wp: error setting wp word"));
}

void print_help()
{
  printf("pedit_cli commands:\n");
  printf("show-text: show text after a specified node\n");
  printf("show-chars: display characters of a ti, thier w, wps and positions\n");
  printf("split-wp: split within a text item, make new word parts\n");
  printf("merge-wp: make several wp of one ti a single wp\n");
  printf("append-wp: add a word part to a word\n");
  printf("separate-wp: separate a word part from a word\n");
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

  if (argc != 4)
    return exerr("usage: <pedit_cli> <database file> <text> <tv>");

  /* open database */
  char *dbfn = argv[1];
  sqlite3 *pDb;
  if (init_database(&pDb, dbfn) != 0)
    return exerr("could not open db");

  /* get text_id */
  char *text_name = argv[2];
  int64_t text_id;
  int text_found;
  if (text_by_name(pDb, text_name, &text_found, &text_id) != 0)
    return exerr("could not find text by name");
  if (!text_found)
    return exerr("could not find text by name");

  /* get tv_id */
  char *tv_name = argv[3];
  int64_t tv_id;
  int tv_found;
  if (tv_by_name(pDb, text_id, tv_name, &tv_found, &tv_id) != 0)
    return exerr("could not find tv by name");
  if (!tv_found)
    return exerr("could not find tv by name 2");

  char *line = NULL;
  size_t size;
  char *args[0x100];
  int argsn;
    printf("->");
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
    if (cmd == NULL || strcmp(cmd, "show-text") == 0) /* word part functions */
      show_text(pDb, text_id, tv_id, argsn, args);
    else if (strcmp(cmd, "show-chars") == 0)
      show_chars(pDb, tv_id, argsn, args);
    else if (strcmp(cmd, "split-wp") == 0)
      split_wp(pDb, argsn, args);
    else if (strcmp(cmd, "merge-wp") == 0)
      merge_wp(pDb, tv_id, argsn, args);
    else if (strcmp(cmd, "append-wp") == 0)
      append_wp(pDb, argsn, args);
    else if (strcmp(cmd, "separate-wp") == 0)
      separate_wp(pDb, argsn, args);
//    else if (...)                                /* text edit functions */
//
//    else if (strcmp(cmd, "show-wordclass") == 0) /* dict struct functions */
//      show_wordclass(pDb, argsn, args); /* wd-cl + cat/val (fix/mov) */
//    else if (strcmp(cmd, "set-wordclass-categories") == 0)
//      set_wordclass_categories(pDb, argsn, args);
//    else if (strcmp(cmd, "set-category-values") == 0)
//      set_category_values(pDb, argsn, args);
//    else if (...)                                /* dict lemma functions */
//
//    else if (...)                                /* text anal functions */
//
    else if (strcmp(cmd, "exit") == 0)
      break;
    else
      print_help();
    printf("->");
  }
  printf("done.\n");

  if (close_database(pDb) != 0)
    return exerr("could not close db");
  return 0;
}
