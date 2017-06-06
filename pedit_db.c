#include <sqlite3.h>
#include <unistd.h>
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
  if (sqlite3_close(pDb) != SQLITE_OK)
    return -1;

  return 0;   
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

int language_get_name(sqlite3 *pDb, int64_t id, char **pname)
{
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
  sprintf(sql, "insert into Text (LanguageID, Name) values (%Ld, '%s');",
                           language_id, name);
  if (sqlite3_exec(pDb, sql, NULL, NULL, NULL) != SQLITE_OK)
    return -1;
  if (get_last_id(pDb, id) != 0)
    return -1;

  return 0;
}

int text_get_language(sqlite3 *pDb, int64_t id, int64_t *language_id)
{
}

int language_by_name(sqlite3 *pDb, char *name, int64_t *id)
{
  char *sql = "select LanguageID from Language WHERE Name = ?;";

  sqlite3_stmt *pStmt;
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK)
    return -1;

  if (sqlite3_bind_text(pStmt, 1, name, -1, NULL) != SQLITE_OK)
    return -1;

  if (sqlite3_step(pStmt) == SQLITE_ROW)
    *id = sqlite3_column_int64(pStmt, 0);
  else
    return -1;

  if (sqlite3_finalize(pStmt) != 0)
    return -1;

  return 0;
}

int text_set_language(sqlite3 *pDb, int64_t id, int64_t language_id)
{
}

int text_get_name(sqlite3 *pDb, int64_t id, char **pname)
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
  if (sqlite3_prepare_v2(pDb, sql, -1, &pStmt, NULL) != SQLITE_OK)
    return -1;

  if (sqlite3_bind_int64(pStmt, 1, text_id) != SQLITE_OK)
    return -1;

  if (sqlite3_bind_text(pStmt, 2, name, -1, NULL) != SQLITE_OK)
    return -1;

  if (sqlite3_step(pStmt) != SQLITE_DONE)
    return -1;

  if (sqlite3_finalize(pStmt) != 0)
    return -1;

  if (get_last_id(pDb, id) != 0)
    return -1;

  return 0;
}
