#include <sys/stat.h>
#include <stdbool.h>

// Returns true if the path exists and is a directory
bool is_directory(const char *path) {
    struct stat path_stat;
    if (stat(path, &path_stat) != 0) return false;
    return (path_stat.st_mode & S_IFDIR) != 0;
}

// Returns true if the path exists and is a regular file
bool is_regular_file(const char *path) {
    struct stat path_stat;
    if (stat(path, &path_stat) != 0) return false;
    return (path_stat.st_mode & S_IFREG) != 0;
}
