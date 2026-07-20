from abc import ABC, abstractmethod
import shlex
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
    """Launch runs as local mpirun processes; drive many concurrently.

    The scheduler packs several runs onto one node at once, so we must stop
    each launcher from pinning its ranks to cores 0..np-1 (the default) — left
    on, 20 concurrent runs would pile onto the first few cores and idle the
    rest of a 92-core node. Binding is disabled via environment variables that
    are MPI-agnostic: each launcher ignores the other's var. FUNWAVE_MPIRUN
    overrides the launcher (e.g. mpiexec.hydra on an Intel-MPI cluster).
    """

    def __init__(self, hpc=None):
        self.jobs = {}
        # On an allocated compute node the whole node is ours -> skip the nice
        # yield that keeps the laptop responsive during interactive work.
        self.hpc = hpc if hpc is not None else bool(os.environ.get("SLURM_JOB_ID") or os.environ.get("PBS_JOBID"))

    def submit(self, binary_path, input_file, work_dir, np=1) -> str:
        env = os.environ.copy()
        # Let the OS scheduler spread packed runs across the node instead of
        # every launcher binding to the same low cores.  Each var is a no-op
        # under the other launchers.  NOTE the OpenMPI 4 -> 5 rename: v5's
        # PRRTE ignores the OMPI_MCA_hwloc_* form, so both spellings are set
        # (missing PRTE_* left packed wheat runs core-stacked ~5x slow).
        env.setdefault("OMPI_MCA_hwloc_base_binding_policy", "none")  # OpenMPI 4
        env.setdefault("PRTE_MCA_hwloc_default_binding_policy", "none")  # OpenMPI 5
        # :OVERSUBSCRIBE — PRRTE also ignores the v4 oversubscribe var and
        # counts physical cores, not hwthreads, so np up to cpu_count needs it
        env.setdefault("PRTE_MCA_rmaps_default_mapping_policy", "node:OVERSUBSCRIBE")  # v5 spread, not core-packed
        env.setdefault("OMPI_MCA_rmaps_base_oversubscribe", "1")
        env.setdefault("I_MPI_PIN", "0")

        launcher = shlex.split(os.environ.get("FUNWAVE_MPIRUN", "mpirun"))
        prefix = [] if self.hpc else ["nice", "-n", "19"]
        cmd = prefix + launcher + ["-np", str(np), binary_path, input_file]
        stdout_path = os.path.join(work_dir, ".mpi_stdout")
        stderr_path = os.path.join(work_dir, ".mpi_stderr")
        stdout_f = open(stdout_path, "w")
        stderr_f = open(stderr_path, "w")
        process = subprocess.Popen(
            cmd,
            cwd=work_dir,
            stdout=stdout_f,
            stderr=stderr_f,
            env=env,
        )
        job_id = f"local_{process.pid}"
        self.jobs[job_id] = {"process": process, "work_dir": work_dir, "stdout_f": stdout_f, "stderr_f": stderr_f}
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
