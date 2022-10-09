#define _GNU_SOURCE
#include "file.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <unistd.h>


int map_file_sentinel(const char *cstr, const uint8_t **view, size_t *len)
{
	int status = -1;
	int err = open(cstr, O_RDONLY);
	int fd = err;
	if (err == -1) goto fail_open;
	struct stat sb;
	err = fstat(fd, &sb);
	if (err == -1) goto fail_open;
	*len = sb.st_size + 0x1000;
	void *base = mmap(NULL, *len, PROT_WRITE, MAP_ANONYMOUS|MAP_PRIVATE, -1, 0);
	if (base == MAP_FAILED) goto fail_open;
	*view = base;
	err = mprotect(base, *len, PROT_READ);
	if (err == -1) goto cleanup;
	base = mmap(base, sb.st_size, PROT_READ, MAP_FIXED|MAP_PRIVATE, fd, 0);
	if (base == MAP_FAILED) goto cleanup;
	status = close(fd);
	goto ok;
cleanup:
	munmap((uint8_t *)*view, *len);
ok:
	close(fd);
fail_open:
	return status;
}

int unmap_file_sentinel(const uint8_t *view, size_t len)
{
	// doesnt matter
	return munmap((uint8_t *)view, len);
}

