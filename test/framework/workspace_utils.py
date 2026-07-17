import os


def get_build_path(workspace_name="dev"):
    work_dir = os.environ.get("FUNWAVE_WORK_DIR", os.path.join(os.getcwd(), "workspaces"))
    return os.path.join(work_dir, workspace_name)


def setup_workspace(workspace_name):
    """
    Creates the workspace directory and a '.workspace_ready' flag.
    """
    path = get_build_path(workspace_name)
    os.makedirs(path, exist_ok=True)

    flag_path = os.path.join(path, ".workspace_ready")
    with open(flag_path, "w") as f:
        f.write(f"Workspace: {workspace_name}\nPath: {path}\n")

    return path
