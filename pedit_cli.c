#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <stdlib.h>
#include <inttypes.h>
#include <setjmp.h>
#include <stdarg.h>

#include <sqlite3.h>

#include "pedit_db.h"
#include "tools/utf8conv.h"

#define SHOW_TEXT_LIM 12

void exerr(char *msg)
{
  fprintf(stderr, "%s\n", msg);
  exit(1);
}

void userr(jmp_buf jbuf, char *msg, ...)
{
  va_list myargs;
  va_start(myargs, msg);

  vprintf(msg, myargs);
  printf("\n");

  va_end(myargs);
  longjmp(jbuf, 77);
}

void get_ti_str(sqlite3 *pDb, int64_t node_id, int64_t tv_id, char *ti_str)
{
  /* get ti id */
  int ti_found;
  int64_t ti_id;
  if (ti_get(pDb, node_id, tv_id, &ti_found, &ti_id) != 0)
    exerr("could not get ti");
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
    exerr("get_ti_str: could not get word parts");
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
      exerr("could not get word id of a word part");
    if (w_found)
      x += sprintf(&ti_str[x], "w%" PRId64 ":", w_id);
    else
      x += sprintf(&ti_str[x], "w[NOPE]:");
    x += sprintf(&ti_str[x], "wp%" PRId64 ":", wps[i]);

    /* get word part text */
    int wptext_found;
    char *wptext;
    if (wp_get_text(pDb, wps[i], &wptext_found, &wptext) != 0)
      exerr("could not get word part text");
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
       exerr("could not get first node");
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
      exerr("could not find whether node exists");
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
      exerr("could not get tc");
    printf("n%" PRId64 "\t<%s>\t%*s\t\t<%s>\n", tn_id, pre,
                                                      32, ti_str, post);

//    printf("%" PRId64 "%s%s%s\n", tn_id, pre, ti_str, post);
    free(pre);
    free(post);
    
    /* find next */
    if (tn_get_next(pDb, tn_id, &found, &tn_id) != 0)
      exerr("could not find next node");
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
    exerr("show_chars: could not find whether node exists");
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
    exerr("could not get ti");
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
    exerr("could not get word parts");
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
      exerr("could not get word part text");
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
    exerr("merge_wp: tn_exist problem");
  if (!node_found)
  {
    printf("wrong node id");
    return;
  }

  /* get wp_text */
  int wptext_found;
  char *wptext;
  if (wp_get_text(pDb, wp_id, &wptext_found, &wptext) != 0)
    exerr("split_wp: could not get wp text");
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
    exerr("split-wp: could not split wp");
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
    exerr("merge_wp: tn_exist problem");
  if (!node_found)
  {
    printf("wrong node id\n");
    return;
  }

  /* get text item */
  int ti_found;
  int64_t ti_id;
  if (ti_get(pDb, tn_id, tv_id, &ti_found, &ti_id) != 0)
    exerr("merge_wp: could not get ti");
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
    exerr("merge_wp: could not get word parts");
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
      exerr("merge_wp: could not get word part text");
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
      exerr("merge_wp: could not get wp w_id");
    if (!w_found)
      exerr("merge_wp: error: wp without w");

    /* delete word part */
    if (i > 0 && wp_delete(pDb, wps[i]) != 0)
      exerr("merge_wp: could not delete word part");

    /* if some word have no more wps, delete => update order! */
    int w_wps_found;
    int w_wps_sz;
    int64_t *w_wps;
    if(wp_get_by_w(pDb, w_id, &w_wps_found, &w_wps_sz, &w_wps) != 0)
      exerr("merge_wp: could not get word wps");
    if (w_wps_found && w_wps_sz == 0)
    {
      if (wp_delete(pDb, w_id) != 0)
        exerr("merge_wp: could not delete word");
    }
  }
  mergedbuf[lentot] = 0;
  
  /* modify wp text */
  if (wp_set_text(pDb, wps[0], mergedbuf) != 0)
    exerr("merge_wp: could not set text");

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
    exerr("append_wp: could not get wp w_id");
  if (!w0_found)
    exerr("append_wp: error: wp without w");

  /* nothing to do if the word id is already the right word id */
  if (w0_id == w_id)
  {
    printf("wp already in w");
    return;
  }
  
  /* update word of the word part */
  if (wp_set_word(pDb, wp_id, w_id) != 0)
    exerr("append_wp: error setting wp word");
  printf("append_wp: update word: w%" PRId64 " on wp%" PRId64 "\n", w_id, wp_id);

  /* get wps of the old word (w0) */
  int wp_count;
  if (word_wp_count(pDb, w0_id, &wp_count) != 0)
    exerr("append_wp: error wp count");

  printf("append_wp: wp_count=%d w0_id=%" PRId64 "\n", wp_count, w0_id);
  /* delete old word if empty */
  if (wp_count == 0 && word_delete(pDb, w0_id) != 0)
    exerr("append_wp: could not delete empty word");
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
    exerr("separate_wp: could not get wp w_id");
  if (!w_found)
    exerr("separate_wp: error: wp without w");

  /* check the number of word parts in its word */
  int wp_count;
  if (word_wp_count(pDb, w_id, &wp_count) != 0)
    exerr("separate_wp: error wp count");

  /* nothing to do if it is the only word part in the word */
  if (wp_count == 1)
    return;
  
  /* create new word */
  int64_t wnew_id;
  if (word_create(pDb, &wnew_id) != 0)
    exerr("separate_wp: error creating new word");

  /* set wp word to new */
  if (wp_set_word(pDb, wp_id, wnew_id) != 0)
    exerr("separate_wp: error setting wp word");
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

void set_wordclass_categories(sqlite3* pDb, 
                              int64_t lang_id, int sz, char *args[])
{
  /* args: <word class> <categories>
   * <category>: f[ixed]:<cat name> or m[oving]:<cat name> */
  if (sz <= 1)
  {
    printf("set-wordclass-categories <word class> <categories>\n"
           "\t<category>: f[ixed]:<cat name> or m[oving]:<cat name>\n");
    return;
  }

  /* getting word class name */
  char *wc_name = args[0];

  /* try to find word class id by word class name and language id */
  int64_t wc_id;
  int wc_found;
  if (wc_get_by_name(pDb, lang_id, wc_name, &wc_found, &wc_id) != 0)
    exerr("could not get wc by name");

  /* if word class does not exists: create a new one */
  if (!wc_found && wc_create(pDb, lang_id, wc_name, &wc_id) != 0)
    exerr("could not create wc");

  for (int i = 1; i < sz; i++)
  {
    /* getting category names and fixedness */
    char *cat = args[i];
    int is_fixed = cat[0] == 'f' ? 1 : 0;
    if (cat[1] != ':' || (cat[0] != 'f' && cat[0] != 'm') || cat[2] == 0)
    {
      printf("bad category: '%s'\n", cat);
      return;
    }
    char *cat_name = &cat[2];

    /* getting category id by name and language id */
    int64_t cat_id;
    int cat_found;
    if (cat_get_by_name(pDb, lang_id, cat_name, &cat_found, &cat_id) != 0)
      exerr("could not get category by name");

    /* if category does not exist, create it */
    if (!cat_found && cat_create(pDb, lang_id, cat_name, &cat_id) != 0)
      exerr("could not create category");

    /* insert into word class category table */
    if (wc_set_cat(pDb, wc_id, cat_id, is_fixed) != 0)
      exerr("could not add word class category");
  }
}

void del_wordclass_category(sqlite3* pDb, 
                              int64_t lang_id, int sz, char *args[])
{
  /* args: <word class> <category> */
  if (sz != 2)
  {
    printf("del-wordclass-category <word class> <category>\n");
    return;
  }

  /* getting word class name */
  char *wc_name = args[0];

  /* try to find word class id by word class name and language id */
  int64_t wc_id;
  int wc_found;
  if (wc_get_by_name(pDb, lang_id, wc_name, &wc_found, &wc_id) != 0)
    exerr("could not get wc by name");

  if (!wc_found)
  {
    printf("word class %s does not exist\n", wc_name);
    return;
  }

  /* getting category name */
  char *cat_name = args[1];

  /* try to find word class id by word class name and language id */
  int64_t cat_id;
  int cat_found;
  if (cat_get_by_name(pDb, lang_id, cat_name, &cat_found, &cat_id) != 0)
    exerr("could not get cat by name");

  if (!cat_found)
  {
    printf("category %s does not exist\n", cat_name);
    return;
  }

  /* delete from word class category table */
  if (wc_del_cat(pDb, wc_id, cat_id) != 0)
    exerr("could not del word class category");
}

void show_wordclass_categories(sqlite3 *pDb,
                              int64_t lang_id, int sz, char *args[])
{
  /* args: <word class> */
  if (sz != 1)
  {
    printf("show-wordclass-categories <word class>\n");
    return;
  }

  /* getting word class name */
  char *wc_name = args[0];

  /* try to find word class id by word class name and language id */
  int64_t wc_id;
  int wc_found;
  if (wc_get_by_name(pDb, lang_id, wc_name, &wc_found, &wc_id) != 0)
    exerr("could not get wc by name");

  if (!wc_found)
  {
    printf("no such word class\n");
    return;
  }

  /* get word class categories */
  int cats_found;
  int cats_sz;
  int64_t *cats;
  if (wc_get_cats(pDb, wc_id, &cats_found, &cats_sz, &cats) != 0)
    exerr("could not get word class categories");

  if (!cats_found)
  {
    printf("no cats found for word class %s\n", wc_name);
    return;
  }

  for (int i = 0; i < cats_sz; i++)
  {
    /* getting category names and fixedness */
    int name_found;
    char *cat_name;
    if (cat_get_name(pDb, cats[i], &name_found, &cat_name) != 0)
      exerr("could not get cat name");
    int fixedness_found;
    int is_fixed;
    if (cat_get_fixedness(pDb, wc_id, cats[i], &fixedness_found, &is_fixed) != 0)
      exerr("could not get cat fixedness");

    printf("%c:%s\n", is_fixed ? 'f' : 'm', cat_name);

    free(cat_name);
  }
  free(cats);
}

void set_category_values(sqlite3* pDb, 
                              int64_t lang_id, int sz, char *args[])
{
  /* args: <category> <category values> */
  if (sz <= 1)
  {
    printf("set-category-values <category> <values>\n");
    return;
  }

  /* getting category name */
  char *cat_name = args[0];

  /* try to find category id by name and language id */
  int64_t cat_id;
  int cat_found;
  if (cat_get_by_name(pDb, lang_id, cat_name, &cat_found, &cat_id) != 0)
    exerr("could not get category by name");

  /* if category does not exists: create a new one */
  if (!cat_found && cat_create(pDb, lang_id, cat_name, &cat_id) != 0)
    exerr("could not create cat");

  for (int i = 1; i < sz; i++)
  {
    /* getting category names and fixedness */
    char *cv_name = args[i];

    /* getting category id by name and language id */
    int64_t cv_id;
    int cv_found;
    if (cv_get_by_name(pDb, cat_id, cv_name, &cv_found, &cv_id) != 0)
      exerr("could not get category value by name");

    /* if value does not exist, create it */
    if (!cv_found && cv_create(pDb, cat_id, cv_name, &cv_id) != 0)
      exerr("could not create category");
  }
}

void del_category_value(sqlite3* pDb, 
                              int64_t lang_id, int sz, char *args[])
{
  /* args: <category> <value> */
  if (sz != 2)
  {
    printf("del-category-value <category> <value>\n");
    return;
  }

  /* getting category name */
  char *cat_name = args[0];

  /* try to find category id by word class name and language id */
  int64_t cat_id;
  int cat_found;
  if (cat_get_by_name(pDb, lang_id, cat_name, &cat_found, &cat_id) != 0)
    exerr("could not get category by name");

  if (!cat_found)
  {
    printf("category %s does not exist\n", cat_name);
    return;
  }

  /* getting category value name */
  char *cv_name = args[1];

  /* try to find category value id by category id and name */
  int64_t cv_id;
  int cv_found;
  if (cv_get_by_name(pDb, cat_id, cv_name, &cv_found, &cv_id) != 0)
    exerr("could not get category value by name");

  if (!cv_found)
  {
    printf("category value %s does not exist\n", cv_name);
    return;
  }

  /* delete from category value */
  if (cv_del(pDb, cv_id) != 0)
    exerr("could not delete category value");
}

void show_category_values(sqlite3 *pDb, int64_t lang_id, int sz, char *args[])
{
  /* args: <category> */
  if (sz != 1)
  {
    printf("show-category-values <category>\n");
    return;
  }

  /* getting category name */
  char *cat_name = args[0];

  /* try to find category id by name and language id */
  int64_t cat_id;
  int cat_found;
  if (cat_get_by_name(pDb, lang_id, cat_name, &cat_found, &cat_id) != 0)
    exerr("could not get category by name");

  /* get category values */
  int cvs_found;
  int cvs_sz;
  int64_t *cvs;
  if (cv_get_by_cat(pDb, cat_id, &cvs_found, &cvs_sz, &cvs) != 0)
    exerr("could not get category values");

  if (!cvs_found)
  {
    printf("no category values found for %s\n", cat_name);
    return;
  }

  for (int i = 0; i < cvs_sz; i++)
  {
    /* getting category value name */
    int cv_name_found;
    char *cv_name;
    if (cv_get_name(pDb, cvs[i], &cv_name_found, &cv_name) != 0)
      exerr("could not get category value name");
    if (!cv_name_found)
      exerr("category value name not found");

    printf("%s\n", cv_name);

    free(cv_name);
  }
  free(cvs);
}

int64_t str2wc(sqlite3 *pDb, jmp_buf jbuf, int64_t lang_id, char *wc_str)
{
  if (wc_str == 0 || wc_str[0] == 0)
    userr(jbuf, "bad word class name");

  int wc_found;
  int64_t wc_id;
  if (wc_get_by_name(pDb, lang_id, wc_str, &wc_found, &wc_id) != 0)
    exerr("str2wc: could not get wc by name");
  if (!wc_found)
    userr(jbuf, "no word category found with name '%s'", wc_str);
  return wc_id;
}

int64_t str2cat(sqlite3 *pDb, jmp_buf jbuf, int64_t lang_id, char *cat_str)
{
  if (cat_str == 0 || cat_str[0] == 0)
    userr(jbuf, "bad category name");

  int64_t cat_id;
  int cat_found;
  if (cat_get_by_name(pDb, lang_id, cat_str, &cat_found, &cat_id) != 0)
    exerr("str2cat: could not find cat by name");
  if (!cat_found)
    userr(jbuf, "no category found with name '%s'", cat_str);
  return cat_id;
}

int64_t str2cv(sqlite3 *pDb, jmp_buf jbuf, int64_t cat_id, char *cv_str)
{
  if (cv_str == 0 || cv_str[0] == 0)
    userr(jbuf, "bad category value");

  int64_t cv_id;
  int cv_found;
  if (cv_get_by_name(pDb, cat_id, cv_str, &cv_found, &cv_id) != 0)
    exerr("str2cv: could not find cv by name");
  if (!cv_found)
    userr(jbuf, "bad category value: '%s'", cv_str);

  return cv_id;
}

void chklem(sqlite3 *pDb, jmp_buf jbuf, int64_t lemma_id)
{
  int lemma_found;
  if (lemma_exists(pDb, lemma_id, &lemma_found) != 0)
    exerr("chcklem: could not find lemma");
  if (!lemma_found)
    userr(jbuf, "no lemma found with id %d\n", lemma_id);
}

void setlemcv(sqlite3 *pDb, jmp_buf jbuf, int64_t lemma_id, int64_t cv_id)
{
  int ok;
  if (lemma_set_cv(pDb, lemma_id, &ok, cv_id) != 0)
    exerr("setlemcv: could not set lemma fixed cv");
  if (!ok)
      userr(jbuf, "setting cv %" PRId64 " not ok for lemma %" PRId64 "\n", cv_id, lemma_id);
}

int64_t newlem(sqlite3 *pDb, jmp_buf jbuf, int64_t wc_id, char *lemma_str)
{
  int64_t lemma_id;
  if (lemma_create(pDb, wc_id, lemma_str, &lemma_id) != 0)
    exerr("could not create new lemma");
  return lemma_id;
}

void chkwd(sqlite3 *pDb, jmp_buf jbuf, int64_t w_id)
{
  int w_found;
  if (w_exists(pDb, w_id, &w_found) != 0)
    exerr("chckwd: could not find word");
  if (!w_found)
    userr(jbuf, "no word found with id %d\n", w_id);
}

void setwdcv(sqlite3 *pDb, jmp_buf jbuf, int64_t w_id, int64_t cv_id)
{
  int ok;
  if (w_set_cv(pDb, w_id, &ok, cv_id) != 0)
    exerr("setwdcv: could not set word moving cv");
  if (!ok)
      userr(jbuf, "setting cv %" PRId64 " not ok for word %" PRId64 "\n", cv_id, w_id);
}

void setwdlem(sqlite3 *pDb, jmp_buf jbuf, int64_t w_id, int64_t lemma_id)
{
  if (w_set_lemma(pDb, w_id, lemma_id) != 0)
    exerr("setwdlem: could not set word lemma");
}

void unsetwdlem(sqlite3 *pDb, jmp_buf jbuf, int64_t w_id)
{
  if (w_unset_lemma(pDb, w_id) != 0)
    exerr("unsetwdlem: could not unset word lemma");
  if (w_del_cvs(pDb, w_id) != 0)
    exerr("unsetwdlem: could not unset word lemma");
}

int64_t getwlem(sqlite3 *pDb, jmp_buf jbuf, int64_t w_id)
{
  int lemma_found;
  int64_t lemma_id;
  if (w_get_lemma(pDb, w_id, &lemma_found, &lemma_id) != 0)
    exerr("getwlem: counld not get word lemma");
  if (!lemma_found)
    userr(jbuf, "could not get lemma for word %" PRId64 "", w_id);
  return lemma_id;
}

char *lem2str(sqlite3 *pDb, jmp_buf jbuf, int64_t lemma_id)
{
  int found;
  char *text;
  if (lemma_get_text(pDb, lemma_id, &found, &text) != 0)
    exerr("lem2str: could not get lemma text");
  if (!found)
    exerr("lem2str: no lemma text found!");
  return text;
}

int64_t *getwcvs(sqlite3 *pDb, jmp_buf jbuf, int64_t word_id)
{
  int found;
  int sz;
  int64_t *cvs;
  if (w_get_cvs(pDb, word_id, &found, &sz, &cvs) != 0)
    exerr("getwcvs: could not get cv name");
  int64_t *res = malloc((sz + 1) * sizeof(int64_t *));
  for (int i = 0; i < sz; i++)
    res[i] = cvs[i];
  res[sz] = 0;
  free(cvs);
  return res;
}

char *cv2str(sqlite3 *pDb, jmp_buf jbuf, int64_t cv_id)
{
  int found;
  char *name;
  if (cv_get_name(pDb, cv_id, &found, &name) != 0)
    exerr("cv2str: could not get cv name");
  if (!found)
    exerr("cv2str: no cv name found!");
  return name;
}

int64_t getcvcat(sqlite3 *pDb, jmp_buf jbuf, int64_t cv_id)
{
  int cat_found;
  int64_t cat_id;
  if (cv_get_cat(pDb, cv_id, &cat_found, &cat_id) != 0)
    exerr("getcvcat: counld not get category value category");
  if (!cat_found)
    userr(jbuf, "could not get category for category value %" PRId64 "", cv_id);
  return cat_id;
}

char *cat2str(sqlite3 *pDb, jmp_buf jbuf, int64_t cat_id)
{
  int found;
  char *name;
  if (cat_get_name(pDb, cat_id, &found, &name) != 0)
    exerr("cat2str: could not get category name");
  if (!found)
    exerr("cat2str: no category name found!");
  return name;
}

/* add lemma <lemma string> <word class> <fixed values> */
void add_lemma(sqlite3 *pDb, jmp_buf jbuf, int64_t lang_id, int sz, char *args[])
{
  /* args: <lemma string> <word class> <fixed values> */
  if (sz < 3)
  {
    printf("add-lemma <lemma string> <word class> <fixed values>\n");
    return;
  }

  int64_t wc_id = str2wc(pDb, jbuf, lang_id, args[1]);
  int64_t lemma_id = newlem(pDb, jbuf, wc_id, args[0]);

  for (int i = 0; i < sz - 2; i++)
  {
    char *cat_str = args[i + 2];
    char *cv_str = 0;
    int split = 0;
    char c;
    while (c = cat_str[split])
    {
      if (c == '=')
      {
        cat_str[split] = 0;
        cv_str = &cat_str[split + 1];
        break;
      }
      split++;
    }
    int64_t cat_id = str2cat(pDb, jbuf, lang_id, cat_str);
    int64_t cv_id = str2cv(pDb, jbuf, cat_id, cv_str);

    setlemcv(pDb, jbuf, lemma_id, cv_id);
  }
}

// TODO: using table WordClassCategory: 
//           * check that only fixed categories are set
//           * check that all fixed categories are set
/* update lemma function -> by id: replace string and fixed values */
void update_lemma(sqlite3 *pDb, jmp_buf jbuf,
                int64_t lang_id, int sz, char *args[])
{
  /* args: <lemma id> <word class> <fixed values> */
  if (sz < 3)
    userr(jbuf, "update-lemma <lemma id> <word class> <fixed values>\n");

  char *lemma_idstr = args[0];
  int64_t lemma_id = atoll(lemma_idstr);

  /* get word class id */
  int64_t wc_id = str2wc(pDb, jbuf, lang_id, args[1]);
  
  /* check if lemma exists */
  chklem(pDb, jbuf, lemma_id);

  for (int i = 0; i < sz - 2; i++)
  {
    char *cat_str = args[i + 2];
    char *cv_str = 0;
    int split = 0;
    char c;
    while (c = cat_str[split])
    {
      if (c == '=')
      {
        cat_str[split] = 0;
        cv_str = &cat_str[split + 1];
        break;
      }
      split++;
    }

    int64_t cat_id = str2cat(pDb, jbuf, lang_id, cat_str);
    int64_t cv_id = str2cv(pDb, jbuf, cat_id, cv_str);

    setlemcv(pDb, jbuf, lemma_id, cv_id);
  }
}

/* delete lemma function -> by id */
void delete_lemma(sqlite3 *pDb, jmp_buf jbuf,int sz, char *args[])
{
  if (sz != 1)
    userr(jbuf, "delete-lemma <lemma id>\n");

  int lemma_found;
  int64_t lemma_id = atoll(args[0]);
  if (lemma_exists(pDb, lemma_id, &lemma_found) != 0)
    exerr("could not create new lemma");
  if (!lemma_found)
    userr(jbuf, "no lemma found with id %d\n", lemma_id);

  if (lemma_delete(pDb, lemma_id) != 0)
    exerr("could not delete lemma");
}

/* show lemmas function -> search by string or substring */
void show_lemmas(sqlite3 *pDb, int64_t lang_id, int sz, char *args[])
{
//TODO: filter by word class / category value / (sub)string
  int lemmas_found;
  int n_lemmas;
  int64_t *lemmas;
  if (lemma_get_all(pDb, lang_id, &lemmas_found, &n_lemmas, &lemmas) != 0)
    exerr("could not get all lemmas");
  if (!lemmas_found)
  {
    printf("no lemma found\n");
    return;
  }
  for (int i = 0; i < n_lemmas; i++)
  {
    int64_t lemma_id = lemmas[i];

    /* get lemma string */
    char *lemma_str;
    int lemma_str_found;
    if (lemma_get_text(pDb, lemma_id, &lemma_str_found, &lemma_str) != 0)
      exerr("show_lemmas: could not get lemma text");
    if (!lemma_str_found)
      exerr("show_lemmas: lemma text not found");

    /* get lemma word class */
    int64_t wc_id;
    int wc_id_found;
    if (lemma_get_wc(pDb, lemma_id, &wc_id_found, &wc_id) != 0)
      exerr("show_lemmas: could not get wc id");
    if (!wc_id_found)
      exerr("show_lemmas: wc id not found");

    /* get word class string */
    char *wc_name;
    int wc_name_found;
    if (wc_get_name(pDb, wc_id, &wc_name_found, &wc_name) != 0)
      exerr("show_lemmas: could not get wc name");
    if (!wc_name_found)
      exerr("show_lemmas: wc name not found");

    printf("%" PRId64 "\t%s (%s): ", lemma_id, lemma_str, wc_name);

    /* get list of lemma fixed categories */
    int cvs_found;
    int n_cvs;
    int64_t *cvs;
    if (lemma_get_fixed_cvs(pDb, lemma_id, &cvs_found, &n_cvs, &cvs) != 0)
      exerr("show_lemmas: could not get lemma fixed category values");
    if (cvs_found)
    {
      for (int j = 0; j < n_cvs; j++)
      {
        int64_t cv_id = cvs[j];

        /* get cv name */
        int cv_name_found;
        char *cv_name;
        if (cv_get_name(pDb, cv_id, &cv_name_found, &cv_name) != 0)
          exerr("show_lemmas: could not get cv name");
        if (!cv_name_found)
          exerr("show_lemmas: cv name not found");

        /* get cv cat */
        int cat_found;
        int64_t cat_id;
        if (cv_get_cat(pDb, cv_id, &cat_found, &cat_id) != 0)
          exerr("show_lemmas: could not get cv cat");
        if (!cat_found)
          exerr("show_lemmas: cv cat not found");

        /* get cat name */
        int cat_name_found;
        char *cat_name;
        if (cat_get_name(pDb, cat_id, &cat_name_found, &cat_name) != 0)
          exerr("show_lemmas: could not get cat name");
        if (!cat_name_found)
          exerr("show_lemmas: cat name not found");

        printf("%s=%s%s", cat_name, cv_name, j < n_cvs - 1 ? "," : "");

        free(cat_name);
        free(cv_name);
      }
      free(cvs);
    }
    printf("\n");
    free(lemma_str);
    free(wc_name);
  }
  free(lemmas);
}

// TODO: using table WordClassCategory: 
//           * check that only moving categories are set
//           * check that all moving categories are set
/* <word id> <lemma id> <moving values> */
void set_word_lemma(sqlite3 *pDb, jmp_buf jbuf, int64_t lang_id, int sz, char *args[])
{
  /* args: <word id> <lemma id> <moving values> */
  if (sz < 3)
    userr(jbuf, "set-word-lemma <word id> <lemma id> <moving values>\n");

  int64_t word_id = atoll(args[0]);
  chkwd(pDb, jbuf, word_id);

  int64_t lemma_id = atoll(args[1]);
  chklem(pDb, jbuf, lemma_id);

  setwdlem(pDb, jbuf, word_id, lemma_id);

  for (int i = 0; i < sz - 2; i++)
  {
    char *cat_str = args[i + 2];
    char *cv_str = 0;
    int split = 0;
    char c;
    while (c = cat_str[split])
    {
      if (c == '=')
      {
        cat_str[split] = 0;
        cv_str = &cat_str[split + 1];
        break;
      }
      split++;
    }

    int64_t cat_id = str2cat(pDb, jbuf, lang_id, cat_str);
    int64_t cv_id = str2cv(pDb, jbuf, cat_id, cv_str);

    setwdcv(pDb, jbuf, word_id, cv_id);
  }
}

/* unset-word-lemma <word_id> */
void unset_word_lemma(sqlite3 *pDb, jmp_buf jbuf, int sz, char *args[])
{
  if (sz != 1)
    userr(jbuf, "unset-word-lemma <word id>\n");

  int64_t word_id = atoll(args[0]);
  chkwd(pDb, jbuf, word_id);

  unsetwdlem(pDb, jbuf, word_id);
}

/* show-word <word_id>: display word lemma and moving values */
void show_word(sqlite3 *pDb, jmp_buf jbuf, int sz, char *args[])
{
  if (sz != 1)
    userr(jbuf, "show-word <word id>\n");

  int64_t word_id = atoll(args[0]);
  chkwd(pDb, jbuf, word_id);

  /* get word lemma */
  int64_t lemma_id = getwlem(pDb, jbuf, word_id);

  /* get lemma string */
  char *lemma_str = lem2str(pDb, jbuf, lemma_id);

  printf("lemma%" PRId64 "=\"%s\". ", lemma_id, lemma_str);

  /* get word moving values */
  int64_t *wcvs = getwcvs(pDb, jbuf, word_id);
  int64_t *pwcv = wcvs;
  while (*pwcv)
  {
    /* get cv string */
    char *cv_str = cv2str(pDb, jbuf, *pwcv);

    /* get cat of the cv */
    int64_t cat_id = getcvcat(pDb, jbuf, *pwcv);

    /* get cat string */
    char *cat_str = cat2str(pDb, jbuf, cat_id);

    printf("%s=%s ", cat_str, cv_str);

    free(cat_str);
    free(cv_str);
    pwcv++;
  }
  printf("\n");
  free(wcvs);
  free(lemma_str);
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
    exerr("usage: <pedit_cli> <database file> <text> <tv>");

  /* open database */
  char *dbfn = argv[1];
  sqlite3 *pDb;
  if (init_database(&pDb, dbfn) != 0)
    exerr("could not open db");

  /* get text_id */
  char *text_name = argv[2];
  int64_t text_id;
  int text_found;
  if (text_by_name(pDb, text_name, &text_found, &text_id) != 0)
    exerr("could not find text by name");
  if (!text_found)
    exerr("could not find text by name");

  /* get language id */
  int64_t lang_id;
  int lang_found;
  if (text_get_language(pDb, text_id, &lang_found, &lang_id) != 0)
    exerr("could not find text language");
  if (!lang_found)
    exerr("could not find text language");

  /* get tv_id */
  char *tv_name = argv[3];
  int64_t tv_id;
  int tv_found;
  if (tv_by_name(pDb, text_id, tv_name, &tv_found, &tv_id) != 0)
    exerr("could not find tv by name");
  if (!tv_found)
    exerr("could not find tv by name 2");

  char *line = NULL;
  size_t size;
  char *args[0x100];
  int argsn;
  jmp_buf jbuf;
  printf("->");
  while (getline(&line, &size, stdin) != -1)
  {
    if (setjmp(jbuf) != 0)
    {
      printf("->");
      continue;
    }

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
    /* words and text functions */
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

    /* dict structure functions */
    else if (strcmp(cmd, "set-wordclass-categories") == 0)
      set_wordclass_categories(pDb, lang_id, argsn, args);
    else if (strcmp(cmd, "del-wordclass-category") == 0)
      del_wordclass_category(pDb, lang_id, argsn, args);
// TODO: allow delete only if category has no values
// TODO: delete word class (allow only no lemmas and categories associated
// TODO: show all word classes
//    else if (strcmp(cmd, "show-wordclasses") == 0)
//      show_wordclasses(pDb, lang_id, argsn, args);
    else if (strcmp(cmd, "show-wordclass-categories") == 0)
      show_wordclass_categories(pDb, lang_id, argsn, args);
    else if (strcmp(cmd, "set-category-values") == 0)
      set_category_values(pDb, lang_id, argsn, args);
    else if (strcmp(cmd, "del-category-value") == 0)
      del_category_value(pDb, lang_id, argsn, args);
// TODO: allow only if value not used as moving or fixed value for word or lemma
    else if (strcmp(cmd, "show-category-values") == 0)
      show_category_values(pDb, lang_id, argsn, args);
    /* dict lemma functions */
    else if (strcmp(cmd, "add-lemma") == 0)
      add_lemma(pDb, jbuf, lang_id, argsn, args);
    // update lemma function -> by id: replace string and fixed values
    else if (strcmp(cmd, "update-lemma") == 0)
      update_lemma(pDb, jbuf, lang_id, argsn, args);
    // delete lemma function -> by id
    else if (strcmp(cmd, "delete-lemma") == 0)
      delete_lemma(pDb, jbuf, argsn, args);
    // show lemmas function -> search by string or substring
    else if (strcmp(cmd, "show-lemmas") == 0)
      show_lemmas(pDb, lang_id, argsn, args);
    /* text anal functions */
    else if (strcmp(cmd, "set-word-lemma") == 0)
      set_word_lemma(pDb, jbuf, lang_id, argsn, args);
    else if (strcmp(cmd, "unset-word-lemma") == 0)
      unset_word_lemma(pDb, jbuf, argsn, args);
    else if (strcmp(cmd, "show-word") == 0)
      show_word(pDb, jbuf, argsn, args);
//
//    else if (...)                                /* text edit functions */
// TODO: update wordpart string
//       insert node -> insert node + word + word part + text item
//       delete word -> delete all word parts of word + lemma + moving values
//                      if text item wothout word parts => delete text item
//       delete node -> only if empty (no text items and no word parts)

    else if (strcmp(cmd, "exit") == 0)
      break;
    else
      print_help();
    printf("->");
  }
  printf("done.\n");

  if (close_database(pDb) != 0)
    exerr("could not close db");
  return 0;
}
