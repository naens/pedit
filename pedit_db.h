#ifndef PEDIT_DB_H
#define PEDIT_DB_H

#include <stdint.h>

/* database functions */
int init_database(sqlite3 **pDb, char *filename);
int close_database(sqlite3 *pDb);

/* language functions */
int language_create(sqlite3 *pDb, char *name, int64_t *id);
int language_get_name(sqlite3 *pDb, int64_t id, int *found, char **pname);
int language_by_name(sqlite3 *pDb, char *name, int *found, int64_t *id);
int language_set_name(sqlite3 *pDb, int64_t id, char *name);
int language_delete(sqlite3 *pDb, int64_t id);

/* text functions */
int text_create(sqlite3 *pDb, int64_t language_id, char *name, int64_t *id);
int text_get_language(sqlite3 *pDb, int64_t id, int *found, int64_t *language_id);
int text_set_language(sqlite3 *pDb, int64_t id, int64_t language_id);
int text_get_name(sqlite3 *pDb, int64_t id, int *found, char **pname);
int text_set_name(sqlite3 *pDb, int64_t id, char *name);
int text_by_name(sqlite3 *pDb, char *name, int *found, int64_t *id);
int text_delete(sqlite3 *pDb, int64_t id);

/* text version functions */
int tv_create(sqlite3 *pDb, int64_t text_id, char *name, int64_t *id);
int tv_by_name(sqlite3 *pDb, int64_t text_id, char *name, int *found, int64_t *id);


/* text node functions */
int tn_create(sqlite3 *pDb, int64_t text_id, int64_t *id);
int tn_delete(sqlite3 *pDb, int64_t id);
int tn_connect(sqlite3 *pDb, int64_t node1id, int64_t node2id);
int tn_disconnect(sqlite3 *pDb, int64_t node1id, int64_t node2id);
int tn_get_prev(sqlite3 *pDb, int64_t node_id, int *found, int64_t *id);
int tn_get_next(sqlite3 *pDb, int64_t node_id, int *found, int64_t *id);
int tn_get_any(sqlite3 *pDb, int64_t text_id, int *found, int64_t *id);
int tn_get_first(sqlite3 *pDb, int64_t text_id, int *found, int64_t *id);
int tn_get_last(sqlite3 *pDb, int64_t text_id, int *found, int64_t *id);
int tn_exists(sqlite3 *pDb, int64_t node_id, int *found);

/* text label functions */

/* text item functions */
int ti_create(sqlite3 *pDb, int64_t node_id, int64_t *id);
int ti_addtv(sqlite3 *pDb, int64_t ti_id, int64_t tv_id);
int ti_deltv(sqlite3 *pDb, int64_t ti_id, int64_t tv_id);

/* text cell functions */
int tc_get(sqlite3 *pDb, int64_t tn_id, int64_t tv_id,
           int *found, char **pre, char **post);
int tc_set(sqlite3 *pDb, int64_t tn_id, int64_t tv_id, char *pre, char *post);

/* text item permutation functions */

/* word functions */
int word_create(sqlite3 *pDb, int64_t *id);

/* word part functions */
int wp_create(sqlite3 *pDb, int64_t ti_id, int64_t w_id,
                 int o_ti, int o_w, char *str, int64_t *id);

#endif /* PEDIT_DB_H */
