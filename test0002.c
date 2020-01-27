/* test0002.c
 * dynamic library for test0002.scm
 */

/* TODO:
 *   open table file in open_dic, close table file in close_dic,
 *   do not open in get_cands.
 *   dynamic memory allocation.
 */

#include <string.h>
#include <stdio.h>
#include "uim.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#include "dynlib.h"
#include <errno.h>
#include <sys/stat.h>

#define ENTRY_MAX_SIZE 0x100
#define CONTENT_MAX_SIZE 0x300
#define CAND_ITEM_MAX 100000

struct cand_item {
	char entry[ENTRY_MAX_SIZE];
	char content[CONTENT_MAX_SIZE];
};

struct cand_list {
	int size;
	struct cand_item item[CAND_ITEM_MAX];
};

static struct cand_list mycand;
static char table_filename[256];

// wildcard match
// return 0 if not matched
// pattern may contain * or ?
static int
match(const char *stroke, const char *pattern)
{
	while (1) {
		if (!*stroke) {
			while (*pattern == '*') { pattern++; }
			return (*pattern == '\0');
		}
		// not matched
		if (!*pattern) { return 0; }

		if (*pattern == '*') {
			// match test for each character
			return (match(stroke, pattern + 1) || match(stroke + 1, pattern));
		}

		if ((*stroke == *pattern) || (*pattern == '?')) {
			stroke++;
			pattern++;
			continue;
		}
		// not matched
		return 0;
	}
}

static void
mycand_look(struct cand_list *pcands, const char *query)
{
	FILE *fp;
	char entry[256];
	char content[256];
	int index = 0;

	fp = fopen(table_filename, "r");
	if (!fp) {
		perror("fopen");
		return;
	}

	char line[256];
	while (fgets(line, sizeof(line), fp)) {
		int n = sscanf(line, "%s %s", entry, content);
		if (n == 2) {
			if (match(entry, query)) {
				strcpy(pcands->item[index].entry, entry);
				strcpy(pcands->item[index].content, content);
				index++;
			}
		}
	}
	pcands->size = index;
	fclose(fp);
}

static uim_lisp
open_dic(uim_lisp filename_)
{
	const char *filename = C_STR(filename_);
	strcpy(table_filename, filename);
	return uim_scm_t();
}

static uim_lisp
close_dic()
{
	table_filename[0] = '\0';
	return uim_scm_t();
}

static uim_lisp
get_cands(uim_lisp str_)
{
	const char *str;
	uim_lisp lst_ = uim_scm_null();

	str = REFER_C_STR(str_);

	mycand_look(&mycand, str);

	struct cand_list *pcands = &mycand;
	for (int i = 0; i < pcands->size; i++) {
		lst_ = CONS(CONS(MAKE_STR(pcands->item[i].entry),
		                 MAKE_STR(pcands->item[i].content)),
		            lst_);
	}

	return uim_scm_callf("reverse", "o", lst_);
}

void
uim_plugin_instance_init(void)
{
	uim_scm_init_proc1("test0002-lib-open-dic", open_dic);
	uim_scm_init_proc0("test0002-lib-close-dic", close_dic);
	uim_scm_init_proc1("test0002-lib-get-cands", get_cands);
}

void
uim_plugin_instance_quit(void)
{
}
