#define _GNU_SOURCE
#include "file.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <unistd.h>


int map_file_sentinel(const char *cstr, const char **view, size_t *len)
{
	int status = IO_OS_ERR;
	if ((status = open(cstr, O_RDONLY)) == -1)
		goto fail_open;
	int fd = status;
	struct stat sb;
	if ((status = fstat(fd, &sb)) == -1)
		goto fail_stat;
	size_t allocate = *len = sb.st_size + 0x1000;
	if (allocate > 1024*1024*1024) {
		status = IO_BIG_FILE;
		goto fail_stat;
	}
	void *base = mmap(NULL, *len, PROT_WRITE, MAP_ANONYMOUS|MAP_PRIVATE, -1, 0);
	if (base == MAP_FAILED)
		goto fail_stat;
	*view = base;
	if ((status = mprotect(base, *len, PROT_READ)) == -1)
		goto cleanup;
	void *file = mmap(base, sb.st_size, PROT_READ, MAP_FIXED|MAP_PRIVATE, fd, 0);
	if (file == MAP_FAILED)
		goto cleanup;
	return close(fd);
cleanup:
	munmap(base, *len);
fail_stat:
	close(fd);
fail_open:
	return status;
}

int unmap_file_sentinel(const char *view, size_t len)
{
	// doesnt matter
	return munmap((char *)view, len);
}

