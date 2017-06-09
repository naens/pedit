#include <sqlite3.h>
#include <unistd.h>
#include <inttypes.h>
#include <stdio.h>

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

int text_get_language(sqlite3 *pDb, int64_t id, int *found, int64_t *language_id)
{
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

int text_delete(sqlite3 *pDb, int64_t id)
{
}

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

/* text cell functions */
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

/* word part functions */
int wp_create(sqlite3 *pDb, int64_t ti_id, int64_t w_id,
                 int o_ti, int o_w, char *str, int64_t *id)
{
  char *sql = "insert into WordPart (TextItemID, WordID, O_TI, O_W, Text) "
              "values (?, ?, ?, ?, ?);";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 1, ti_id) != SQLITE_OK
     || sqlite3_bind_int64(pStmt, 2, w_id) != SQLITE_OK
     || sqlite3_bind_int(pStmt, 3, o_ti) != SQLITE_OK
     || sqlite3_bind_int(pStmt, 4, o_w) != SQLITE_OK
     || sqlite3_bind_text(pStmt, 5, str, -1, NULL) != SQLITE_OK
     || sqlite3_step(pStmt) != SQLITE_DONE
     || sqlite3_finalize(pStmt) != 0
     || get_last_id(pDb, id) != 0)
    return -1;

  return 0;
}
