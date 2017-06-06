#ifndef PEDIT_DB_H
#define PEDIT_DB_H

#include <stdint.h>

/* database functions */
int init_database(char *filename);

/* language functions */
int language_create(char *name);
int language_get_name(int64_t id, char **pname);
int language_set_name(int64_t id, char *name);
int language_delete(int64_t id);

/* text functions */
int text_create(int64_t language_id, char *name);
int text_get_language(int64_t id, int64_t *language_id);
int text_set_language(int64_t id, int64_t language_id);
int text_get_name(int64_t id, char **pname);
int text_set_name(int64_t id, char *name);
int text_delete(int64_t id);

/* text version functions */

/* text node functions */

/* text label functions */

/* text item functions */

/* text item permutation functions */

#endif /* PEDIT_DB_H */
