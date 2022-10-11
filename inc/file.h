#ifndef CROUTE_FILE_H
#define CROUTE_FILE_H

#include <stddef.h>
#include <stdint.h>


enum io_status {
	IO_OS_ERR = -1,
	IO_OK = 0,
	IO_BIG_FILE = 1,
};

int map_file_sentinel(const char *cstr, const char **view, size_t *len);
int unmap_file_sentinel(const char *view, size_t len);

#endif /* CROUTE_FILE_H */

