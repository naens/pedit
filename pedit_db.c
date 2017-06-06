#include "pedit_db.h"

/* database functions */
int init_database(char *filename)
{
}

/* language functions */
int language_create(char *name)
{
}

int language_get_name(int64_t id, char **pname)
{
}

int language_set_name(int64_t id, char *name)
{
}

int language_delete(int64_t id)
{
}

/* text functions */
int text_create(int64_t language_id, char *name)
{
}

int text_get_language(int64_t id, int64_t *language_id)
{
}

int text_set_language(int64_t id, int64_t language_id)
{
}

int text_get_name(int64_t id, char **pname)
{
}

int text_set_name(int64_t id, char *name)
{
}

int text_delete(int64_t id)
{
}
