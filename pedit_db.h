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
int tn_connect(sqlite3 *pDb, int64_t tn1_id, int64_t tn2_id);
int tn_disconnect(sqlite3 *pDb, int64_t tn1_id, int64_t tn2_id);
int tn_get_prev(sqlite3 *pDb, int64_t tn_id, int *found, int64_t *id);
int tn_get_next(sqlite3 *pDb, int64_t tn_id, int *found, int64_t *id);
int tn_get_any(sqlite3 *pDb, int64_t text_id, int *found, int64_t *id);
int tn_get_first(sqlite3 *pDb, int64_t text_id, int *found, int64_t *id);
int tn_get_last(sqlite3 *pDb, int64_t text_id, int *found, int64_t *id);
int tn_exists(sqlite3 *pDb, int64_t tn_id, int *found);

/* text label functions */

/* text item functions */
int ti_create(sqlite3 *pDb, int64_t node_id, int64_t *id);
int ti_addtv(sqlite3 *pDb, int64_t ti_id, int64_t tv_id);
int ti_deltv(sqlite3 *pDb, int64_t ti_id, int64_t tv_id);
int ti_get(sqlite3 *pDb, int64_t tn_id, int64_t tv_id, int *found, int64_t *id);

/* text cell functions */
int tc_get(sqlite3 *pDb, int64_t tn_id, int64_t tv_id,
           int *found, char **pre, char **post);
int tc_set(sqlite3 *pDb, int64_t tn_id, int64_t tv_id, char *pre, char *post);

/* text item permutation functions */

/* word functions */
int word_create(sqlite3 *pDb, int64_t *id);
int word_delete(sqlite3 *pDb, int64_t w_id);
int word_wp_count(sqlite3 *pDb, int64_t w_id, int *count);

/* word part functions */
int wp_create(sqlite3 *pDb, int64_t ti_id, int64_t w_id,
                 int ti_o, char *str, int64_t *id);
int wp_delete(sqlite3 *pDb, int64_t wp_id);
int wp_split(sqlite3 *pDb, int64_t wp_id,
                 char *text1, char *text2, int64_t *id);
int wp_get_by_ti(sqlite3 *pDb, int64_t ti_id,
                 int *found, int *sz, int64_t **wps);
int wp_get_by_w(sqlite3 *pDb, int64_t w_id,
                 int *found, int *sz, int64_t **wps);
int wp_get_word(sqlite3 *pDb, int64_t wp_id, int *found, int64_t *id);
int wp_get_ti(sqlite3 *pDb, int64_t wp_id, int *found, int64_t *id);
int wp_get_text(sqlite3 *pDb, int64_t wp_id, int *found, char **text);
int wp_get_ti_o(sqlite3 *pDb, int64_t wp_id, int *found, int *ti_o);
int wp_set_text(sqlite3 *pDb, int64_t wp_id, char *text);
int wp_set_word(sqlite3 *pDb, int64_t wp_id, int64_t w_id);

/* word class functions */
int wc_create(sqlite3 *pDb, int64_t lang_id, char *name, int64_t *id);
int wc_get_by_name(sqlite3 *pDb,
                     int64_t lang_id, char *name, int *found, int64_t *id);
int wc_get_cats(sqlite3 *pDb,
                     int64_t wc_id, int *found, int *sz, int64_t **cats);
int wc_set_cat(sqlite3 *pDb, int64_t wc_id, int64_t cat_id, int fixed);
int wc_del_cat(sqlite3 *pDb, int64_t wc_id, int64_t cat_id);

/* category functions */
int cat_create(sqlite3 *pDb, int64_t lang_id, char *name, int64_t *id);
int cat_get_by_name(sqlite3 *pDb, int64_t lang_id, char *name, int *found, int64_t *id);
int cat_get_name(sqlite3 *pDb, int64_t cat_id, int *found, char **name);
int cat_get_fixedness(sqlite3 *pDb, int64_t wc_id, int64_t cat_id,
                      int *found, int *is_fixed);

/* category value functions */
int cv_create(sqlite3 *pDb, int64_t cat_id, char *name, int64_t *id);
int cv_get_by_name(sqlite3 *pDb, int64_t cat_id, char *name, int *found, int64_t *id);
int cv_get_by_cat(sqlite3 *pDb,
                     int64_t cat_id, int *found, int *sz, int64_t **cvs);
int cv_get_name(sqlite3 *pDb, int64_t cat_id, int *found, char **name);
int cv_del(sqlite3 *pDb, int64_t cv_id);

#endif /* PEDIT_DB_H */
