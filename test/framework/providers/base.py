from abc import ABC, abstractmethod
import subprocess
import os
from pathlib import Path

class BaseProvider(ABC):
    @abstractmethod
    def submit(self, binary_path, input_file, work_dir, np=1) -> str:
        """Submit a job and return a job_id."""
        pass

    @abstractmethod
    def get_status(self, job_id: str) -> str:
        """Return the current status: QUEUED, RUNNING, COMPLETED, FAILED."""
        pass

class LocalProvider(BaseProvider):
    def __init__(self):
        self.jobs = {}

    def submit(self, binary_path, input_file, work_dir, np=1) -> str:
        cmd = ["mpirun", "-np", str(np), binary_path, input_file]
        stdout_path = os.path.join(work_dir, ".mpi_stdout")
        stderr_path = os.path.join(work_dir, ".mpi_stderr")
        stdout_f = open(stdout_path, "w")
        stderr_f = open(stderr_path, "w")
        process = subprocess.Popen(
            cmd,
            cwd=work_dir,
            stdout=stdout_f,
            stderr=stderr_f,
        )
        job_id = f"local_{process.pid}"
        self.jobs[job_id] = {"process": process, "work_dir": work_dir,
                             "stdout_f": stdout_f, "stderr_f": stderr_f}
        return job_id

    def get_status(self, job_id: str) -> str:
        entry = self.jobs.get(job_id)
        if not entry:
            return "FAILED"
        returncode = entry["process"].poll()
        if returncode is None:
            return "RUNNING"
        entry["stdout_f"].close()
        entry["stderr_f"].close()
        return "COMPLETED" if returncode == 0 else "FAILED"

    def get_output(self, job_id: str) -> tuple[str, str]:
        entry = self.jobs.get(job_id)
        if not entry:
            return "", ""
        work_dir = entry["work_dir"]
        def _read(name):
            p = Path(work_dir) / name
            return p.read_text() if p.exists() else ""
        return _read(".mpi_stdout"), _read(".mpi_stderr")
