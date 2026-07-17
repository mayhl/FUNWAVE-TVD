#!/usr/bin/env python3
"""
Patch rdtscp → rdtsc+nop in Intel MPI shared library.

Docker Desktop on Mac doesn't virtualise rdtscp even though CPUID reports it
available, causing SIGILL in MPL_wtime_init. rdtsc works fine; the processor
ID written to ECX by rdtscp is unused in Intel MPI's timing code.

  rdtscp:   0f 01 f9  (3 bytes)
  rdtsc:    0f 31     (2 bytes)
  nop:      90        (1 byte)
"""

import ctypes
import glob
import sys

pattern = bytes([0x0F, 0x01, 0xF9])
replacement = bytes([0x0F, 0x31, 0x90])


def rdtscp_works() -> bool:
    """Return True if rdtscp executes without SIGILL (fork-probe)."""
    import ctypes.util, os

    src = b"\x0f\x01\xf9\xc3"  # rdtscp; ret
    pid = os.fork()
    if pid == 0:
        buf = ctypes.create_string_buffer(src)
        libc = ctypes.CDLL(ctypes.util.find_library("c"))
        libc.mprotect.restype = ctypes.c_int
        page = ctypes.addressof(buf) & ~0xFFF
        libc.mprotect(ctypes.c_void_p(page), 8192, 7)  # PROT_READ|WRITE|EXEC
        ctypes.CFUNCTYPE(None)(ctypes.addressof(buf))()
        os._exit(0)
    _, status = os.waitpid(pid, 0)
    return os.WIFEXITED(status) and os.WEXITSTATUS(status) == 0


if rdtscp_works():
    print("rdtscp works natively — skipping patch")
    sys.exit(0)

print("rdtscp causes SIGILL — patching Intel MPI")

paths = sys.argv[1:] or glob.glob("/opt/intel/oneapi/mpi/latest/lib/release/libmpi.so*")

for path in paths:
    with open(path, "rb") as f:
        data = bytearray(f.read())
    n = 0
    pos = 0
    while True:
        idx = data.find(pattern, pos)
        if idx < 0:
            break
        data[idx : idx + 3] = replacement
        n += 1
        pos = idx + 3
    with open(path, "wb") as f:
        f.write(data)
    print(f"{path}: patched {n} rdtscp occurrences")
