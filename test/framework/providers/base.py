from abc import ABC, abstractmethod
import subprocess
import os

class BaseProvider(ABC):
    @abstractmethod
    def submit(self, binary_path, input_file, work_dir) -> str:
        """Submit a job and return a job_id."""
        pass

    @abstractmethod
    def get_status(self, job_id: str) -> str:
        """Return the current status: QUEUED, RUNNING, COMPLETED, FAILED."""
        pass

class LocalProvider(BaseProvider):
    def __init__(self, mpi_np=2):
        self.mpi_np = mpi_np
        self.jobs = {}

    def submit(self, binary_path, input_file, work_dir) -> str:
        cmd = ["mpirun", "-np", str(self.mpi_np), binary_path, input_file]
        
        # In local mode, we fire and forget (mocking async behavior)
        process = subprocess.Popen(
            cmd, 
            cwd=work_dir, 
            stdout=subprocess.PIPE, 
            stderr=subprocess.PIPE,
            text=True
        )
        job_id = f"local_{process.pid}"
        self.jobs[job_id] = process
        return job_id

    def get_status(self, job_id: str) -> str:
        process = self.jobs.get(job_id)
        if not process:
            return "FAILED"
        
        returncode = process.poll()
        if returncode is None:
            return "RUNNING"
        return "COMPLETED" if returncode == 0 else "FAILED"
