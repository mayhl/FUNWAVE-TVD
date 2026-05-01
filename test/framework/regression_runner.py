import time
import os
import shutil
import subprocess
from test.framework.base_runner import BaseRunner

class RegressionRunner(BaseRunner):
    def __init__(self, reporter, provider, ref_branch="master"):
        super().__init__(reporter)
        self.provider = provider
        self.ref_branch = ref_branch
        self.repo_root = os.getcwd()
        # Ensure we are in a clean state and can perform git operations
        try:
            self.is_ref_same_as_head = (subprocess.check_output(["git", "rev-parse", "--abbrev-ref", "HEAD"]).decode().strip() == ref_branch)
        except:
            self.is_ref_same_as_head = False
        
        self.worktree_path = os.path.join(self.repo_root, "test", "regression", "worktrees", ref_branch)
        self.ref_build_dir = os.path.join(self.worktree_path, "build_ref")
        self.curr_build_dir = os.path.join(self.repo_root, "build_curr")

    def _prepare_environment(self):
        if self.is_ref_same_as_head:
            self.reporter.info("Reference branch is same as current. Using isolated directories.")
            self.worktree_path = self.repo_root
        else:
            if os.path.exists(self.worktree_path):
                self.reporter.error(f"Worktree already exists at {self.worktree_path}.")
                self.reporter.error("Please clean up existing worktrees using 'scripts/manage_worktrees.sh' before running regression tests.")
                return False
            
            self.reporter.step(f"Creating isolated worktree for branch: {self.ref_branch}")
            subprocess.run(["git", "worktree", "add", self.worktree_path, self.ref_branch], 
                           check=True, capture_output=True)
        return True

    def _build(self, build_dir, source_dir):
        if os.path.exists(build_dir):
            shutil.rmtree(build_dir)
        os.makedirs(build_dir)
        
        self.reporter.info(f"Building {source_dir} -> {build_dir}")
        toolchain_path = os.path.join(self.repo_root, "macos_mpi.cmake")
        cmake_cmd = ["cmake", "-S", source_dir, "-B", build_dir, f"-DCMAKE_TOOLCHAIN_FILE={toolchain_path}", "-DENABLE_TESTING=ON"]
        
        from rich.progress import Progress, SpinnerColumn, TextColumn, BarColumn, TaskProgressColumn
        with Progress(SpinnerColumn(), TextColumn("[progress.description]{task.description}"), transient=True) as progress:
            # Remove the configuration task
            config_task = progress.add_task(f"Configuring {os.path.basename(build_dir)}...", total=None)
            subprocess.run(cmake_cmd, check=True, capture_output=True)
            progress.remove_task(config_task)
            
            # Estimate compilation progress by monitoring make output
            build_task = progress.add_task(f"Compiling {os.path.basename(build_dir)}...", total=100)
            make_proc = subprocess.Popen(["make", "-C", build_dir, "-j8"], 
                                         stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
            
            while make_proc.poll() is None:
                line = make_proc.stdout.readline()
                if "[" in line and "%" in line:
                    try:
                        percent = int(line.split("[")[1].split("%")[0].strip())
                        progress.update(build_task, completed=percent)
                    except: pass
            
            if make_proc.returncode != 0:
                raise subprocess.CalledProcessError(make_proc.returncode, "make")
            
        self.reporter.success(f"Build complete in {build_dir}")

    def run(self):
        self.reporter.step(f"Running Regression Tests (Ref: {self.ref_branch})")
        if not self._prepare_environment(): return

        # Build in isolated directories
        self._build(self.ref_build_dir, self.worktree_path)
        self._build(self.curr_build_dir, self.repo_root)

        # Simulation metadata
        simulations = [
            {"name": "wave_prop", "input": "input.yaml", "binary": "exe_funwave"}
        ]

        # Trigger jobs
        jobs = []
        for sim in simulations:
            ref_bin = os.path.join(self.ref_build_dir, sim["binary"])
            curr_bin = os.path.join(self.curr_build_dir, sim["binary"])
            
            ref_id = self.provider.submit(ref_bin, sim["input"], self.worktree_path)
            curr_id = self.provider.submit(curr_bin, sim["input"], self.repo_root)
            jobs.append({"name": sim["name"], "ref_id": ref_id, "curr_id": curr_id})

        # Monitor loop
        self.reporter.info("Monitoring simulation jobs...")
        while any(self.provider.get_status(j["ref_id"]) not in ["COMPLETED", "FAILED"] or 
                  self.provider.get_status(j["curr_id"]) not in ["COMPLETED", "FAILED"] for j in jobs):
            time.sleep(2)
        
        self.reporter.success("Regression simulations complete.")
