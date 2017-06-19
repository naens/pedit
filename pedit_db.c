#include <unistd.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>

#include <sqlite3.h>

#include "pedit_db.h"
#include "create_db.h"

/* database functions */
int init_database(sqlite3 **ppDb, char *filename)
{
  int new_db = (access(filename, 0) != 0);
  if (sqlite3_open(filename, ppDb) != SQLITE_OK)
    return -1;

  /* execute database creation script if no db file found */
  if (new_db && sqlite3_exec(*ppDb, create_db, NULL, NULL, NULL) != SQLITE_OK)
    return -1;

  return 0;
}

int close_database(sqlite3 *pDb)
{
  int rc = sqlite3_close(pDb);
  if (rc == SQLITE_OK)
    return 0;
  else if (rc == SQLITE_BUSY)
  {
    fprintf(stderr, "close db: busy\n");
    return -1;
  }
  else
    return -1;
}

char *make_string(int sz, const char *str)
{
  char *res = malloc(sz+1);
  for (int i = 0; i < sz; i++)
    res[i] = str[i];
  res[sz] = 0;
  return res;
}

int get_last_id(sqlite3 *pDb, int64_t *id)
{
  char *sql = "SELECT last_insert_rowid();";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK)
    return -1;

  if (sqlite3_step(pStmt) == SQLITE_ROW)
    *id = sqlite3_column_int64(pStmt, 0);

  if (sqlite3_finalize(pStmt) != 0)
    return -1;

  return 0;
}

/* language functions */
int language_create(sqlite3 *pDb, char *name, int64_t *id)
{
  char sql[0x1000];
  sprintf(sql, "insert into Language (Name) values ('%s');", name);
  if (sqlite3_exec(pDb, sql, NULL, NULL, NULL) != SQLITE_OK)
    return -1;
  if (get_last_id(pDb, id) != 0)
    return -1;

  return 0;
}

int language_get_name(sqlite3 *pDb, int64_t id, int *found, char **pname)
{
}

int language_by_name(sqlite3 *pDb, char *name, int *found, int64_t *id)
{
  char *sql = "select LanguageID from Language WHERE Name = ?;";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_text(pStmt, 1, name, -1, NULL) != SQLITE_OK)
    return -1;

  int rc = sqlite3_step(pStmt);
  *found = (rc == SQLITE_ROW);
  *id = (rc == SQLITE_ROW ? sqlite3_column_int64(pStmt, 0) : 0);
  if (rc == SQLITE_DONE || rc == SQLITE_ROW)
  {
    if (sqlite3_finalize(pStmt) != 0)
      return -1;
    return 0;
  } 
  else
  {
    sqlite3_finalize(pStmt);
    return -1;
  }
}

int language_set_name(sqlite3 *pDb, int64_t id, char *name)
{
}

int language_delete(sqlite3 *pDb, int64_t id)
{
}

/* text functions */
int text_create(sqlite3 *pDb, int64_t language_id, char *name, int64_t *id)
{
  char sql[0x1000];
  sprintf(sql, "insert into Text (LanguageID, Name) values (%" PRId64 ", '%s');",
                           language_id, name);
  if (sqlite3_exec(pDb, sql, NULL, NULL, NULL) != SQLITE_OK)
    return -1;
  if (get_last_id(pDb, id) != 0)
    return -1;

  return 0;
}

int text_get_language(sqlite3 *pDb, int64_t text_id, int *found, int64_t *id)
{
  char *sql = "select LanguageID from Text where TextID = ?;";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, text_id) != SQLITE_OK)
    return -1;

  int rc = sqlite3_step(pStmt);
  *found = (rc == SQLITE_ROW);
  *id = (rc == SQLITE_ROW ? sqlite3_column_int64(pStmt, 0) : 0);
  if (rc == SQLITE_DONE || rc == SQLITE_ROW)
  {
    if (sqlite3_finalize(pStmt) != 0)
      return -1;
    return 0;
  } 
  else
  {
    sqlite3_finalize(pStmt);
    return -1;
  }
}

int text_set_language(sqlite3 *pDb, int64_t id, int64_t language_id)
{
}

int text_get_name(sqlite3 *pDb, int64_t id, int *found, char **pname)
{
}

int text_set_name(sqlite3 *pDb, int64_t id, char *name)
{
}

int text_by_name(sqlite3 *pDb, char *name, int *found, int64_t *id)
{
  char *sql = "select TextID from Text WHERE Name = ?;";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_text(pStmt, 1, name, -1, NULL) != SQLITE_OK)
    return -1;

  int rc = sqlite3_step(pStmt);
  *found = (rc == SQLITE_ROW);
  *id = (rc == SQLITE_ROW ? sqlite3_column_int64(pStmt, 0) : 0);
  if (rc == SQLITE_DONE || rc == SQLITE_ROW)
  {
    if (sqlite3_finalize(pStmt) != 0)
      return -1;
    return 0;
  } 
  else
  {
    sqlite3_finalize(pStmt);
    return -1;
  }
}

int text_delete(sqlite3 *pDb, int64_t id)
{
}

/* text version functions */
int tv_create(sqlite3 *pDb, int64_t text_id, char *name, int64_t *id)
{
  char *sql = "insert into TextVersion (TextID, Name) values (?, ?);";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, text_id) != SQLITE_OK
     || sqlite3_bind_text(pStmt, 2, name, -1, NULL) != SQLITE_OK
     || sqlite3_step(pStmt) != SQLITE_DONE
     || sqlite3_finalize(pStmt) != 0
     || get_last_id(pDb, id) != 0)
    return -1;

  return 0;
}

int tv_by_name(sqlite3 *pDb, int64_t text_id, char *name,
              int *found, int64_t *id)
{
  char *sql = "select TextVersionID from TextVersion "
              "WHERE TextID = ? AND Name = ?;";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, text_id) != SQLITE_OK
     || sqlite3_bind_text(pStmt, 2, name, -1, NULL) != SQLITE_OK)
    return -1;

  int rc = sqlite3_step(pStmt);
  *found = (rc == SQLITE_ROW);
  *id = (rc == SQLITE_ROW ? sqlite3_column_int64(pStmt, 0) : 0);
  if (rc == SQLITE_DONE || rc == SQLITE_ROW)
  {
    if (sqlite3_finalize(pStmt) != 0)
      return -1;
    return 0;
  } 
  else
  {
    sqlite3_finalize(pStmt);
    return -1;
  }
}


/* text node functions */

int tn_create(sqlite3 *pDb, int64_t text_id, int64_t *id)
{
  char *sql = "insert into TextNode (TextID) values (?);";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, text_id) != SQLITE_OK
     || sqlite3_step(pStmt) != SQLITE_DONE
     || sqlite3_finalize(pStmt) != 0
     || get_last_id(pDb, id) != 0)
    return -1;

  return 0;
}

int tn_delete(sqlite3 *pDb, int64_t id)
{
}

int tn_connect(sqlite3 *pDb, int64_t node1id, int64_t node2id)
{
  char *sql = "insert into TextNodeConnection "
              "(TextNodeFromID, TextNodeToID) values (?, ?);";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, node1id) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 2, node2id) != SQLITE_OK
     || sqlite3_step(pStmt) != SQLITE_DONE
     || sqlite3_finalize(pStmt) != 0)
    return -1;

  return 0;
}

int tn_disconnect(sqlite3 *pDb, int64_t node1id, int64_t node2id)
{
}

int tn_get_prev(sqlite3 *pDb, int64_t node_id, int *found, int64_t *id)
{
}

int tn_get_next(sqlite3 *pDb, int64_t node_id, int *found, int64_t *id)
{
  char *sql = "select TextNodeToID from TextNodeConnection "
              "WHERE TextNodeFromID = ?;";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, node_id) != SQLITE_OK)
    return -1;

  int rc = sqlite3_step(pStmt);
  if (*found = (rc == SQLITE_ROW))
    *id = sqlite3_column_int64(pStmt, 0);

  if (sqlite3_finalize(pStmt) != 0)
    return -1;

  return 0;
}

int tn_get_any(sqlite3 *pDb, int64_t text_id, int *found, int64_t *id)
{
  char *sql = "select TextNodeID from TextNode WHERE TextID = ?;";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, text_id) != SQLITE_OK)
    return -1;

  int rc = sqlite3_step(pStmt);
  if (*found = (rc == SQLITE_ROW))
    *id = sqlite3_column_int64(pStmt, 0);

  if (sqlite3_finalize(pStmt) != 0)
    return -1;

  return 0;
}


int tn_get_first(sqlite3 *pDb, int64_t text_id, int *found, int64_t *id)
{
  char *sql = "select TextNodeFromID from TextNodeConnection "
              "JOIN TextNode on TextNodeFromID = TextNodeID "
              "WHERE TextID = ? AND TextNodeToID NOT IN "
              "(select TextNodeToID from TextNodeConnection);";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, text_id) != SQLITE_OK)
    return -1;

  int rc = sqlite3_step(pStmt);
  if (rc == SQLITE_DONE)
  {
    if (tn_get_any(pDb, text_id, found, id) != 0)
      return -1;
  }
  else if (*found = (rc == SQLITE_ROW))
    *id = sqlite3_column_int64(pStmt, 0);
  else
    return -1;

  if (sqlite3_finalize(pStmt) != 0)
    return -1;

  return 0;
}

int tn_get_last(sqlite3 *pDb, int64_t text_id, int *found, int64_t *id)
{
  char *sql = "select TextNodeToID from TextNodeConnection "
              "JOIN TextNode on TextNodeToID = TextNodeID "
              "WHERE TextID = ? AND TextNodeToID NOT IN "
              "(select TextNodeFromID from TextNodeConnection);";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, text_id) != SQLITE_OK)
    return -1;

  int rc = sqlite3_step(pStmt);
  if (rc == SQLITE_DONE)
  {
    if (tn_get_any(pDb, text_id, found, id) != 0)
      return -1;
  }
  else if (*found = (rc == SQLITE_ROW))
    *id = sqlite3_column_int64(pStmt, 0);
  else
    return -1;

  if (sqlite3_finalize(pStmt) != 0)
    return -1;

  return 0;
}

int tn_exists(sqlite3 *pDb, int64_t node_id, int *found)
{
  char *sql = "select TextNodeID from TextNode where TextNodeID = ?;";
  
  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, node_id) != SQLITE_OK)
    return -1;
  int rc = sqlite3_step(pStmt);
  if (rc == SQLITE_ROW)
    *found = 1;
  else if (rc == SQLITE_DONE)
    *found = 0;
  else
    return -1;
    
  if (sqlite3_finalize(pStmt) != 0)
    return -1;

  return 0;
}

/* text label functions */

/* text item functions */
int ti_create(sqlite3 *pDb, int64_t tn_id, int64_t *id)
{
  char *sql = "insert into TextItem (TextNodeID) values (?);";
  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, tn_id) != SQLITE_OK
     || sqlite3_step(pStmt) != SQLITE_DONE
     || sqlite3_finalize(pStmt) != 0
     || get_last_id(pDb, id) != 0)
    return -1;

  return 0;
}

int ti_addtv(sqlite3 *pDb, int64_t ti_id, int64_t tv_id)
{
  char *sql = "insert into TextItemTextVersion "
              "(TextItemID, TextVersionID) values (?, ?)";
  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, ti_id) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 2, tv_id) != SQLITE_OK
     || sqlite3_step(pStmt) != SQLITE_DONE
     || sqlite3_finalize(pStmt) != 0)
    return -1;

  return 0;
}

int ti_deltv(sqlite3 *pDb, int64_t ti_id, int64_t tv_id)
{
}

int ti_get(sqlite3 *pDb, int64_t tn_id, int64_t tv_id, int *found, int64_t *id)
{
  char *sql = "select TextItem.TextItemID from "
              "TextItem join TextItemTextVersion "
              "on TextItem.TextItemID = TextItemTextVersion.TextItemID "
              "where TextItem.TextNodeID = ? "
              "and TextItemTextVersion.TextVersionID = ?;";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, tn_id) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 2, tv_id) != SQLITE_OK)
    return -1;

  int rc = sqlite3_step(pStmt);
  *found = (rc == SQLITE_ROW);
  int res = (rc == SQLITE_ROW || rc == SQLITE_DONE) ? 0 : -1;
  if (rc == SQLITE_ROW)
    *id = sqlite3_column_int64(pStmt, 0);

  if (sqlite3_finalize(pStmt) != 0)
    return -1;

  return res;
}

/* text cell functions */
int tc_get(sqlite3 *pDb, int64_t tn_id, int64_t tv_id,
             int *found, char **pre, char **post)
{
  char *sql = "select Pre, Post from TextCell "
              "WHERE TextNodeID = ? AND TextVersionID = ?;";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, tn_id) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 2, tv_id) != SQLITE_OK)
    return -1;

  int rc = sqlite3_step(pStmt);
  if (rc == SQLITE_DONE)
  {
    *found = 0;
  }
  else if (rc == SQLITE_ROW)
  {
    *found = 1;
    const char *pre_tmp = sqlite3_column_text(pStmt, 0);
    int pre_sz = sqlite3_column_bytes(pStmt, 0);
    *pre = make_string(pre_sz, pre_tmp);
    const char *post_tmp = sqlite3_column_text(pStmt, 1);
    int post_sz = sqlite3_column_bytes(pStmt, 1);
    *post = make_string(post_sz, post_tmp);
  }
  else
    return -1;

  if (sqlite3_finalize(pStmt) != 0)
    return -1;

  return 0;
}

int tc_set(sqlite3 *pDb, int64_t tn_id, int64_t tv_id, char *pre, char *post)
{
  char *sql = "insert into TextCell (TextNodeID, TextVersionID, Pre, Post) "
              "values (?, ?, ?, ?);";
  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, tn_id) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 2, tv_id) != SQLITE_OK
     || sqlite3_bind_text(pStmt, 3, pre, -1, NULL) != SQLITE_OK
     || sqlite3_bind_text(pStmt, 4, post, -1, NULL) != SQLITE_OK
     || sqlite3_step(pStmt) != SQLITE_DONE
     || sqlite3_finalize(pStmt) != 0)
    return -1;

  return 0;
}

/* word functions */
int word_create(sqlite3 *pDb, int64_t *id)
{
  char *sql = "insert into Word DEFAULT VALUES;";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_step(pStmt) != SQLITE_DONE
     || sqlite3_finalize(pStmt) != 0
     || get_last_id(pDb, id) != 0)
    return -1;

  return 0;
}

int word_delete(sqlite3 *pDb, int64_t w_id)
{
  char *sql = "delete from Word where WordID = ?;";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, w_id) != SQLITE_OK
     || sqlite3_step(pStmt) != SQLITE_DONE
     || sqlite3_finalize(pStmt) != 0)
    return -1;

  return 0;
}

int word_wp_count(sqlite3 *pDb, int64_t w_id, int *count)
{
  char *sql = "select count(*) from WordPart where WordID = ?";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, w_id) != SQLITE_OK)
    return -1;

  if (sqlite3_step(pStmt) != SQLITE_ROW)
    return -1;

  *count = sqlite3_column_int(pStmt, 0);

  if (sqlite3_finalize(pStmt) != 0)
    return -1;

  return 0;
}

/* word part functions */
int wp_create(sqlite3 *pDb, int64_t ti_id, int64_t w_id,
                 int o_ti, char *str, int64_t *id)
{
  char *sql = "insert into WordPart (TextItemID, WordID, TI_O, Text) "
              "values (?, ?, ?, ?);";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, ti_id) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 2, w_id) != SQLITE_OK
     || sqlite3_bind_int(pStmt, 3, o_ti) != SQLITE_OK
     || sqlite3_bind_text(pStmt, 4, str, -1, NULL) != SQLITE_OK
     || sqlite3_step(pStmt) != SQLITE_DONE
     || sqlite3_finalize(pStmt) != 0
     || get_last_id(pDb, id) != 0)
    return -1;

  return 0;
}

int wp_delete(sqlite3 *pDb, int64_t wp_id)
{
  int ti_o_found;
  int ti_o;
  if (wp_get_ti_o(pDb, wp_id, &ti_o_found, &ti_o) != 0)
    return -1;
  if (!ti_o_found)
    return -1;

  int w_found;
  int64_t w_id;
  if (wp_get_word(pDb, wp_id, &w_found, &w_id) != 0)
    return -1;
  if (!w_found)
    return -1;

  int ti_found;
  int64_t ti_id;
  if (wp_get_ti(pDb, wp_id, &ti_found, &ti_id) != 0)
    return -1;
  if (!ti_found)
    return -1;

  sqlite3_stmt *pStmt;
  char *sql="update WordPart set TI_O = TI_O - 1 where TextItemID = ? and TI_O > ?; ";
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, ti_id) != SQLITE_OK
     || sqlite3_bind_int(pStmt, 2, ti_o) != SQLITE_OK
     || sqlite3_step(pStmt) != SQLITE_DONE
     || sqlite3_finalize(pStmt) != 0)
    return -1;

  sql = "delete from WordPart where WordPartID = ?;";
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, wp_id) != SQLITE_OK
     || sqlite3_step(pStmt) != SQLITE_DONE
     || sqlite3_finalize(pStmt) != 0)
    return -1;

  return 0;
}

/* create new wp with same ti and w, updating w_o and ti_o */
int wp_split(sqlite3 *pDb, int64_t wp_id,
                 char *text1, char *text2, int64_t *id)
{
  fprintf(stderr, "wp_split(0): wp_id=%" PRId64 ", text1='%s' text2='%s'\n",
                   wp_id, text1, text2);
  sqlite3_stmt *pStmt;
  /* get values for the wp_id */
  char *sql = "select WordID, TextItemID, TI_O from WordPart "
              "where WordPartID = ?;";
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, wp_id) != SQLITE_OK)
    return -1;
  if (sqlite3_step(pStmt) != SQLITE_ROW)
    return -1;
  int64_t w_id = sqlite3_column_int64(pStmt, 0);
  int64_t ti_id = sqlite3_column_int64(pStmt, 1);
  int ti_o = sqlite3_column_int(pStmt, 2);
  if (sqlite3_finalize(pStmt) != 0)
    return -1;

  sql="update WordPart set TI_O = TI_O + 1 where TextItemID = ? and TI_O > ?; ";
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, ti_id) != SQLITE_OK
     || sqlite3_bind_int(pStmt, 2, ti_o) != SQLITE_OK
     || sqlite3_step(pStmt) != SQLITE_DONE
     || sqlite3_finalize(pStmt) != 0)
    return -1;
  
  if (wp_set_text(pDb, wp_id, text1) != 0)
    return -1;

  /* insert new wp */
  if (wp_create(pDb, ti_id, w_id, ti_o + 1, text2, id) != 0)
    return -1;
  return 0;
}


int wp_get_by_ti(sqlite3 *pDb, int64_t ti_id,
                 int *found, int *sz, int64_t **wps)
{
  char *sql = "select WordPartID from WordPart where TextItemID = ? "
              "order by TI_O;";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, ti_id) != SQLITE_OK)
    return -1;

  int rc;
  int count = 0;
  while ((rc = sqlite3_step(pStmt)) == SQLITE_ROW)
    count++;

  if (count > 0)
  {
    sqlite3_reset(pStmt);
    *wps = malloc(sizeof(int64_t *) * count);
    int i = 0;
    while ((rc = sqlite3_step(pStmt)) == SQLITE_ROW)
      (*wps)[i++] = sqlite3_column_int64(pStmt, 0);
  }

  *found = (count > 0);
  *sz = count;
  int res = (rc == SQLITE_DONE) ? 0 : -1;

  if (sqlite3_finalize(pStmt) != 0)
    return -1;

  return res;
}

int wp_get_by_w(sqlite3 *pDb, int64_t w_id,
                 int *found, int *sz, int64_t **wps)
{
  char *sql = "select WordPartID from WordPart where WordID = ? ";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, w_id) != SQLITE_OK)
    return -1;

  int rc;
  int count = 0;
  while ((rc = sqlite3_step(pStmt)) == SQLITE_ROW)
    count++;

  if (count > 0)
  {
    sqlite3_reset(pStmt);
    *wps = malloc(sizeof(int64_t *) * count);
    int i = 0;
    while ((rc = sqlite3_step(pStmt)) == SQLITE_ROW)
      (*wps)[i++] = sqlite3_column_int64(pStmt, 0);
  }

  *found = (count > 0);
  *sz = count;
  int res = (rc == SQLITE_DONE) ? 0 : -1;

  if (sqlite3_finalize(pStmt) != 0)
    return -1;

  return res;
}

int wp_get_word(sqlite3 *pDb, int64_t wp_id, int *found, int64_t *id)
{
  char *sql = "select WordID from WordPart where WordPartID = ?;";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, wp_id) != SQLITE_OK)
    return -1;

  int rc = sqlite3_step(pStmt);
  *found = (rc == SQLITE_ROW);
  int res = (rc == SQLITE_ROW || rc == SQLITE_DONE) ? 0 : -1;
  if (rc == SQLITE_ROW)
    *id = sqlite3_column_int64(pStmt, 0);

  if (sqlite3_finalize(pStmt) != 0)
    return -1;

  return res;
}

int wp_get_ti(sqlite3 *pDb, int64_t wp_id, int *found, int64_t *id)
{
  char *sql = "select TextItemID from WordPart where WordPartID = ?;";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, wp_id) != SQLITE_OK)
    return -1;

  int rc = sqlite3_step(pStmt);
  *found = (rc == SQLITE_ROW);
  int res = (rc == SQLITE_ROW || rc == SQLITE_DONE) ? 0 : -1;
  if (rc == SQLITE_ROW)
    *id = sqlite3_column_int64(pStmt, 0);

  if (sqlite3_finalize(pStmt) != 0)
    return -1;

  return res;
}

int wp_get_text(sqlite3 *pDb, int64_t wp_id, int *found, char **text)
{
  char *sql = "select WordPart.Text from WordPart where WordPartID = ?;";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, wp_id) != SQLITE_OK)
    return -1;

  int rc = sqlite3_step(pStmt);
  *found = (rc == SQLITE_ROW);
  int res = (rc == SQLITE_ROW || rc == SQLITE_DONE) ? 0 : -1;
  if (rc == SQLITE_ROW)
  {
    const char *tmp = sqlite3_column_text(pStmt, 0);
    int sz = sqlite3_column_bytes(pStmt, 0);
    *text = make_string(sz, tmp);
  }

  if (sqlite3_finalize(pStmt) != 0)
    return -1;

  return res;
}

int wp_get_ti_o(sqlite3 *pDb, int64_t wp_id, int *found, int *ti_o)
{
  char *sql = "select TI_O from WordPart where WordPartID = ?;";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, wp_id) != SQLITE_OK)
    return -1;

  int rc = sqlite3_step(pStmt);
  if (*found = (rc == SQLITE_ROW))
    *ti_o = sqlite3_column_int(pStmt, 0);

  if (sqlite3_finalize(pStmt) != 0)
    return -1;

  return 0;
}

int wp_set_text(sqlite3 *pDb, int64_t wp_id, char *text)
{
  char *sql  = "update WordPart set Text = ? where WordPartID = ?;";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_text(pStmt, 1, text, -1, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 2, wp_id) != SQLITE_OK
     || sqlite3_step(pStmt) != SQLITE_DONE
     || sqlite3_finalize(pStmt) != 0)
    return -1;

  return 0;
}

int wp_set_word(sqlite3 *pDb, int64_t wp_id, int64_t w_id)
{
  char *sql  = "update WordPart set WordId = ? where WordPartID = ?;";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, w_id) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 2, wp_id) != SQLITE_OK
     || sqlite3_step(pStmt) != SQLITE_DONE
     || sqlite3_finalize(pStmt) != 0)
    return -1;

  return 0;
}

int wc_create(sqlite3 *pDb, int64_t lang_id, char *name, int64_t *id)
{
  char *sql = "insert into WordClass (LanguageID, Name) values (?, ?);";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, lang_id) != SQLITE_OK
     || sqlite3_bind_text(pStmt, 2, name, -1, NULL) != SQLITE_OK
     || sqlite3_step(pStmt) != SQLITE_DONE
     || sqlite3_finalize(pStmt) != 0
     || get_last_id(pDb, id) != 0)
    return -1;

  return 0;
}

int wc_get_by_name(sqlite3 *pDb, int64_t lang_id, char *name, int *found, int64_t *id)
{
  char *sql = "select WordClassID from WordClass "
              "where LanguageID = ? AND Name = ?;";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, lang_id) != SQLITE_OK
     || sqlite3_bind_text(pStmt, 2, name, -1, NULL) != SQLITE_OK)
    return -1;

  int rc = sqlite3_step(pStmt);
  *found = (rc == SQLITE_ROW);
  *id = (rc == SQLITE_ROW ? sqlite3_column_int64(pStmt, 0) : 0);
  if (rc == SQLITE_DONE || rc == SQLITE_ROW)
  {
    if (sqlite3_finalize(pStmt) != 0)
      return -1;
    return 0;
  } 
  else
  {
    sqlite3_finalize(pStmt);
    return -1;
  }
}

int wc_get_cats(sqlite3 *pDb,
                     int64_t wc_id, int *found, int *sz, int64_t **cats)
{
  char *sql = "select CategoryID from WordClassCategory "
              "where WordClassID = ?";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, wc_id) != SQLITE_OK)
    return -1;

  int rc;
  int count = 0;
  while ((rc = sqlite3_step(pStmt)) == SQLITE_ROW)
    count++;

  if (count > 0)
  {
    sqlite3_reset(pStmt);
    *cats = malloc(sizeof(int64_t *) * count);
    int i = 0;
    while ((rc = sqlite3_step(pStmt)) == SQLITE_ROW)
      (*cats)[i++] = sqlite3_column_int64(pStmt, 0);
  }

  *found = (count > 0);
  *sz = count;
  int res = (rc == SQLITE_DONE) ? 0 : -1;

  if (sqlite3_finalize(pStmt) != 0)
    return -1;

  return res;
}

int wc_get_name(sqlite3 *pDb, int64_t wc_id, int *found, char **name)
{
  char *sql = "select Name from WordClass where WordClassID = ?;";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, wc_id) != SQLITE_OK)
    return -1;

  int rc = sqlite3_step(pStmt);
  *found = (rc == SQLITE_ROW);
  int res = (rc == SQLITE_ROW || rc == SQLITE_DONE) ? 0 : -1;
  if (rc == SQLITE_ROW)
  {
    const char *tmp = sqlite3_column_text(pStmt, 0);
    int sz = sqlite3_column_bytes(pStmt, 0);
    *name = make_string(sz, tmp);
  }

  if (sqlite3_finalize(pStmt) != 0)
    return -1;

  return res;
}

int wc_set_cat(sqlite3 *pDb, int64_t wc_id, int64_t cat_id, int fixed)
{
  char *sql = "insert or replace into WordClassCategory "
              "(WordClassID, CategoryID, Fixed) values (?, ?, ?);";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, wc_id) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 2, cat_id) != SQLITE_OK
     || sqlite3_bind_int(pStmt, 3, fixed) != SQLITE_OK /* SQL: BOOLEAN, C: int */
     || sqlite3_step(pStmt) != SQLITE_DONE
     || sqlite3_finalize(pStmt) != 0)
    return -1;

  return 0;
}

int wc_del_cat(sqlite3 *pDb, int64_t wc_id, int64_t cat_id)
{
  char *sql = "delete from WordClassCategory "
              "where WordClassID = ? AND CategoryId = ?;";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, wc_id) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 2, cat_id) != SQLITE_OK
     || sqlite3_step(pStmt) != SQLITE_DONE
     || sqlite3_finalize(pStmt) != 0)
    return -1;

  return 0;
}
/* category functions */
int cat_create(sqlite3 *pDb, int64_t lang_id, char *name, int64_t *id)
{
  char *sql = "insert into Category (LanguageID, Name) values (?, ?);";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, lang_id) != SQLITE_OK
     || sqlite3_bind_text(pStmt, 2, name, -1, NULL) != SQLITE_OK
     || sqlite3_step(pStmt) != SQLITE_DONE
     || sqlite3_finalize(pStmt) != 0
     || get_last_id(pDb, id) != 0)
    return -1;

  return 0;
}

int cat_get_by_name(sqlite3 *pDb, int64_t lang_id, char *name, int *found, int64_t *id)
{
  char *sql = "select CategoryID from Category "
              "where LanguageID = ? AND Name = ?;";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, lang_id) != SQLITE_OK
     || sqlite3_bind_text(pStmt, 2, name, -1, NULL) != SQLITE_OK)
    return -1;

  int rc = sqlite3_step(pStmt);
  *found = (rc == SQLITE_ROW);
  *id = (rc == SQLITE_ROW ? sqlite3_column_int64(pStmt, 0) : 0);
  if (rc == SQLITE_DONE || rc == SQLITE_ROW)
  {
    if (sqlite3_finalize(pStmt) != 0)
      return -1;
    return 0;
  } 
  else
  {
    sqlite3_finalize(pStmt);
    return -1;
  }
}

int cat_get_name(sqlite3 *pDb, int64_t cat_id, int *found, char **name)
{
  char *sql = "select Name from Category where CategoryID = ?;";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, cat_id) != SQLITE_OK)
    return -1;

  int rc = sqlite3_step(pStmt);
  *found = (rc == SQLITE_ROW);
  int res = (rc == SQLITE_ROW || rc == SQLITE_DONE) ? 0 : -1;
  if (rc == SQLITE_ROW)
  {
    const char *tmp = sqlite3_column_text(pStmt, 0);
    int sz = sqlite3_column_bytes(pStmt, 0);
    *name = make_string(sz, tmp);
  }

  if (sqlite3_finalize(pStmt) != 0)
    return -1;

  return res;
}

int cat_get_fixedness(sqlite3 *pDb, int64_t wc_id, int64_t cat_id,
                      int *found, int *is_fixed)
{
  char *sql = "select Fixed from WordClassCategory "
              "where WordClassID = ? AND CategoryID = ?;";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, wc_id) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 2, cat_id) != SQLITE_OK)
    return -1;

  int rc = sqlite3_step(pStmt);
  *found = (rc == SQLITE_ROW);
  int res = (rc == SQLITE_ROW || rc == SQLITE_DONE) ? 0 : -1;
  if (rc == SQLITE_ROW)
    *is_fixed = sqlite3_column_int(pStmt, 0);

  if (sqlite3_finalize(pStmt) != 0)
    return -1;

  return res;
}

int cv_create(sqlite3 *pDb, int64_t cat_id, char *name,
                                                                int64_t *id)
{
  char *sql = "insert into CategoryValue (CategoryId, Name) "
              "values (?, ?);";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, cat_id) != SQLITE_OK
     || sqlite3_bind_text(pStmt, 2, name, -1, NULL) != SQLITE_OK
     || sqlite3_step(pStmt) != SQLITE_DONE
     || sqlite3_finalize(pStmt) != 0
     || get_last_id(pDb, id) != 0)
    return -1;

  return 0;
}

int cv_get_by_name(sqlite3 *pDb, int64_t cat_id, char *name,
                                                     int *found, int64_t *id)
{
  char *sql = "select CategoryValueID from CategoryValue "
              "where CategoryID = ? AND Name = ?;";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, cat_id) != SQLITE_OK
     || sqlite3_bind_text(pStmt, 2, name, -1, NULL) != SQLITE_OK)
    return -1;

  int rc = sqlite3_step(pStmt);
  *found = (rc == SQLITE_ROW);
  *id = (rc == SQLITE_ROW ? sqlite3_column_int64(pStmt, 0) : 0);
  if (rc == SQLITE_DONE || rc == SQLITE_ROW)
  {
    if (sqlite3_finalize(pStmt) != 0)
      return -1;
    return 0;
  } 
  else
  {
    sqlite3_finalize(pStmt);
    return -1;
  }
}

int cv_get_by_cat(sqlite3 *pDb,
                     int64_t cat_id, int *found, int *sz, int64_t **cvs)
{
  char *sql = "select CategoryValueID from CategoryValue "
              "where CategoryID = ?";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, cat_id) != SQLITE_OK)
    return -1;

  int rc;
  int count = 0;
  while ((rc = sqlite3_step(pStmt)) == SQLITE_ROW)
    count++;

  if (count > 0)
  {
    sqlite3_reset(pStmt);
    *cvs = malloc(sizeof(int64_t *) * count);
    int i = 0;
    while ((rc = sqlite3_step(pStmt)) == SQLITE_ROW)
      (*cvs)[i++] = sqlite3_column_int64(pStmt, 0);
  }

  *found = (count > 0);
  *sz = count;
  int res = (rc == SQLITE_DONE) ? 0 : -1;

  if (sqlite3_finalize(pStmt) != 0)
    return -1;

  return res;
}

int cv_get_name(sqlite3 *pDb, int64_t cv_id, int *found, char **name)
{
  char *sql = "select Name from CategoryValue where CategoryValueID = ?;";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, cv_id) != SQLITE_OK)
    return -1;

  int rc = sqlite3_step(pStmt);
  *found = (rc == SQLITE_ROW);
  int res = (rc == SQLITE_ROW || rc == SQLITE_DONE) ? 0 : -1;
  if (rc == SQLITE_ROW)
  {
    const char *tmp = sqlite3_column_text(pStmt, 0);
    int sz = sqlite3_column_bytes(pStmt, 0);
    *name = make_string(sz, tmp);
  }

  if (sqlite3_finalize(pStmt) != 0)
    return -1;

  return res;
}

int cv_get_cat(sqlite3 *pDb, int64_t cv_id, int *found, int64_t *cat_id)
{
  char *sql = "select CategoryID from CategoryValue where CategoryValueID = ?;";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, cv_id) != SQLITE_OK)
    return -1;

  int rc = sqlite3_step(pStmt);
  *found = (rc == SQLITE_ROW);
  int res = (rc == SQLITE_ROW || rc == SQLITE_DONE) ? 0 : -1;
  if (rc == SQLITE_ROW)
    *cat_id = sqlite3_column_int64(pStmt, 0);

  if (sqlite3_finalize(pStmt) != 0)
    return -1;

  return res;
}

int cv_del(sqlite3 *pDb, int64_t cv_id)
{
  char *sql = "delete from CategoryValue where CategoryValueID = ?;";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, cv_id) != SQLITE_OK
     || sqlite3_step(pStmt) != SQLITE_DONE
     || sqlite3_finalize(pStmt) != 0)
    return -1;

  return 0;
}

int lemma_create(sqlite3 *pDb, int64_t wc_id, char *text, int64_t *id)
{
  char *sql = "insert into Lemma (WordClassId, Text) values (?, ?);";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, wc_id) != SQLITE_OK
     || sqlite3_bind_text(pStmt, 2, text, -1, NULL) != SQLITE_OK
     || sqlite3_step(pStmt) != SQLITE_DONE
     || sqlite3_finalize(pStmt) != 0
     || get_last_id(pDb, id) != 0)
    return -1;

  return 0;
}

int lemma_exists(sqlite3 *pDb, int64_t lemma_id, int *found)
{
  char *sql = "select LemmaID from Lemma where LemmaID = ?;";
  
  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, lemma_id) != SQLITE_OK)
    return -1;
  int rc = sqlite3_step(pStmt);
  if (rc == SQLITE_ROW)
    *found = 1;
  else if (rc == SQLITE_DONE)
    *found = 0;
  else
    return -1;
    
  if (sqlite3_finalize(pStmt) != 0)
    return -1;

  return 0;
}

int lemma_delete(sqlite3 *pDb, int64_t lemma_id)
{
  char *sql = "delete from Lemma where LemmaID = ?;";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, lemma_id) != SQLITE_OK
     || sqlite3_step(pStmt) != SQLITE_DONE
     || sqlite3_finalize(pStmt) != 0)
    return -1;

  return 0;
}

int lemma_set_cv(sqlite3 *pDb, int64_t lemma_id, int *ok, int64_t cv_id)
{
  int cv_found;
  int64_t cat_id;
  if (cv_get_cat(pDb, cv_id, &cv_found, &cat_id) != 0)
    return -1;
  if (!cv_found)
    return -1;

  int wc_found;
  int64_t wc_id;
  if (lemma_get_wc(pDb, lemma_id, &wc_found, &wc_id) != 0)
    return -1;
  if (!wc_found)
    return -1;

  int fixedness_found;
  int is_fixed;
  if (cat_get_fixedness(pDb, wc_id, cat_id, &fixedness_found, &is_fixed) != 0)
    return -1;
  if (!fixedness_found)
    return -1;
  if (!is_fixed)
  {
    *ok = 0;
    return 0;
  }

  if (lemma_del_cat(pDb, lemma_id, cat_id) != 0)
    return -1;

  if (lemma_add_cv(pDb, lemma_id, cv_id) != 0)
    return -1;

  *ok = 1;
  return 0;
}

int lemma_del_cat(sqlite3 *pDb, int64_t lemma_id, int64_t cat_id)
{
  char *sql = "delete from LemmaFixedValue "
          "where LemmaID = ? AND CategoryValueID IN "
          "(select CategoryValueID from CategoryValue where CategoryID = ?)";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, lemma_id) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 2, cat_id) != SQLITE_OK
     || sqlite3_step(pStmt) != SQLITE_DONE
     || sqlite3_finalize(pStmt) != 0)
    return -1;

  return 0;
}

int lemma_add_cv(sqlite3 *pDb, int64_t lemma_id, int64_t cv_id)
{
  char *sql = "insert into LemmaFixedValue (LemmaID, CategoryValueID) "
              "values (?, ?);";
  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, lemma_id) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 2, cv_id) != SQLITE_OK
     || sqlite3_step(pStmt) != SQLITE_DONE
     || sqlite3_finalize(pStmt) != 0)
    return -1;

  return 0;
}

int lemma_get_all(sqlite3 *pDb, int64_t lang_id,
                      int *found, int *sz, int64_t **lemmas)
{
  char *sql = "select LemmaID from Lemma "
              "join WordClass on Lemma.WordClassId = WordClass.WordClassID "
              "where WordClass.LanguageID = ?;";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, lang_id) != SQLITE_OK)
    return -1;

  int rc;
  int count = 0;
  while ((rc = sqlite3_step(pStmt)) == SQLITE_ROW)
    count++;

  if (count > 0)
  {
    sqlite3_reset(pStmt);
    *lemmas = malloc(sizeof(int64_t *) * count);
    int i = 0;
    while ((rc = sqlite3_step(pStmt)) == SQLITE_ROW)
      (*lemmas)[i++] = sqlite3_column_int64(pStmt, 0);
  }

  *found = (count > 0);
  *sz = count;
  int res = (rc == SQLITE_DONE) ? 0 : -1;

  if (sqlite3_finalize(pStmt) != 0)
    return -1;

  return res;
}

int lemma_get_text(sqlite3 *pDb, int64_t lemma_id, int *found, char **text)
{
  char *sql = "select Text from Lemma where LemmaID = ?;";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, lemma_id) != SQLITE_OK)
    return -1;

  int rc = sqlite3_step(pStmt);
  *found = (rc == SQLITE_ROW);
  int res = (rc == SQLITE_ROW || rc == SQLITE_DONE) ? 0 : -1;
  if (rc == SQLITE_ROW)
  {
    const char *tmp = sqlite3_column_text(pStmt, 0);
    int sz = sqlite3_column_bytes(pStmt, 0);
    *text = make_string(sz, tmp);
  }

  if (sqlite3_finalize(pStmt) != 0)
    return -1;

  return res;
}

int lemma_get_wc(sqlite3 *pDb, int64_t lemma_id, int *found, int64_t *wc_id)
{
  char *sql = "select WordClassID from Lemma where LemmaID = ?;";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, lemma_id) != SQLITE_OK)
    return -1;

  int rc = sqlite3_step(pStmt);
  *found = (rc == SQLITE_ROW);
  int res = (rc == SQLITE_ROW || rc == SQLITE_DONE) ? 0 : -1;
  if (rc == SQLITE_ROW)
    *wc_id = sqlite3_column_int64(pStmt, 0);

  if (sqlite3_finalize(pStmt) != 0)
    return -1;

  return res;
}

int lemma_unset_fixed_cvs(sqlite3 *pDb, int64_t lemma_id)
{
  char *sql = "delete from LemmaFixedValue where LemmaID = ?;";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, lemma_id) != SQLITE_OK
     || sqlite3_step(pStmt) != SQLITE_DONE
     || sqlite3_finalize(pStmt) != 0)
    return -1;

  return 0;
}

int lemma_get_fixed_cvs(sqlite3 *pDb, int64_t lemma_id,
                     int *found, int *sz, int64_t **cvs)
{
  char *sql = "select CategoryValueID from LemmaFixedValue "
              "where LemmaID = ?;";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, lemma_id) != SQLITE_OK)
    return -1;

  int rc;
  int count = 0;
  while ((rc = sqlite3_step(pStmt)) == SQLITE_ROW)
    count++;

  if (count > 0)
  {
    sqlite3_reset(pStmt);
    *cvs = malloc(sizeof(int64_t *) * count);
    int i = 0;
    while ((rc = sqlite3_step(pStmt)) == SQLITE_ROW)
      (*cvs)[i++] = sqlite3_column_int64(pStmt, 0);
  }

  *found = (count > 0);
  *sz = count;
  int res = (rc == SQLITE_DONE) ? 0 : -1;

  if (sqlite3_finalize(pStmt) != 0)
    return -1;

  return res;
}
