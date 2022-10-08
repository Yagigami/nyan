#ifndef CROUTE_FILE_H
#define CROUTE_FILE_H

#include <stddef.h>
#include <stdint.h>


int cr_map_file_sentinel(const char *cstr, const uint8_t **view, size_t *len);
int cr_unmap_file_sentinel(const uint8_t *view, size_t len);

#endif /* CROUTE_FILE_H */

