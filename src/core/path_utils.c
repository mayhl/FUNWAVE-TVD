#include <sys/stat.h>
#include <stdbool.h>
#include <sys/types.h>
#include <unistd.h>

#ifdef _WIN32
#include <direct.h>
#define MKDIR(path) _mkdir(path)
#else
#include <sys/stat.h>
#define MKDIR(path) mkdir(path, 0755)
#endif

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

// Returns true if directory creation succeeded
bool mkdir_wrapper(const char *path) {
    return MKDIR(path) == 0;
}

// Returns true if directory removal succeeded
bool rmdir_wrapper(const char *path) {
#ifdef _WIN32
    return _rmdir(path) == 0;
#else
    return rmdir(path) == 0;
#endif
}
