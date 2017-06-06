#ifndef PEDIT_DB_H
#define PEDIT_DB_H

#include <stdint.h>

/* database functions */
int init_database(sqlite3 **pDb, char *filename);
int close_database(sqlite3 *pDb);

/* language functions */
int language_create(sqlite3 *pDb, char *name, int64_t *id);
int language_get_name(sqlite3 *pDb, int64_t id, char **pname);
int language_by_name(sqlite3 *pDb, char *name, int64_t *id);
int language_set_name(sqlite3 *pDb, int64_t id, char *name);
int language_delete(sqlite3 *pDb, int64_t id);

/* text functions */
int text_create(sqlite3 *pDb, int64_t language_id, char *name, int64_t *id);
int text_get_language(sqlite3 *pDb, int64_t id, int64_t *language_id);
int text_set_language(sqlite3 *pDb, int64_t id, int64_t language_id);
int text_get_name(sqlite3 *pDb, int64_t id, char **pname);
int text_set_name(sqlite3 *pDb, int64_t id, char *name);
int text_delete(sqlite3 *pDb, int64_t id);

/* text version functions */
int tv_create(sqlite3 *pDb, int64_t text_id, char *name, int64_t *id);

/* text node functions */

/* text label functions */

/* text item functions */

/* text item permutation functions */

#endif /* PEDIT_DB_H */
