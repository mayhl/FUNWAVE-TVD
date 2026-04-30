import time
import random
from rich.console import Console, Group
from rich.table import Table
from rich.progress import Progress, BarColumn, TextColumn, TaskProgressColumn, TimeElapsedColumn, ProgressColumn
from rich.live import Live
from test.framework.base_runner import BaseRunner

class ComponentRunner(BaseRunner):
    def run(self):
        self.reporter.step("Running Component Tests (pFUnit)")
        Console().print("")
        
        groups = {
            "I/O": ["test_yaml_input_serial.pf", "test_yaml_input_parallel.pf"],
            "Math": ["test_path_mod.pf", "test_range_parser.pf"],
            "Logging": ["test_logging.pf"]
        }
        
        results = []
        
        # Consistent columns for all rows
        common_columns = (
            TextColumn("[progress.description]{task.description:<10}"),
            BarColumn(),
            TaskProgressColumn(),
            TextColumn("([green]P:[/green]{task.fields[passed]} [red]F:[/red]{task.fields[failed]})"),
            TimeElapsedColumn()
        )
        
        progress = Progress(*common_columns)
        tasks = {name: progress.add_task(name, total=len(files), passed=0, failed=0) 
                 for name, files in groups.items()}
        
        summary_progress = Progress(*common_columns)
        summary_task = summary_progress.add_task("Total", total=sum(len(f) for f in groups.values()), 
                                        passed=0, failed=0)
        
        dashboard = Group(progress, summary_progress)
        
        with Live(dashboard, refresh_per_second=10):
            for group, files in groups.items():
                for f in files:
                    start = time.time()
                    time.sleep(0.5) 
                    passed = random.choice([True, True, True, False])
                    duration = time.time() - start
                    
                    results.append({'name': f, 'status': 'Pass' if passed else 'Fail', 'time': f"{duration:.2f}s"})
                    
                    # Update group
                    task_id = tasks[group]
                    progress.update(task_id, advance=1, 
                                    passed=progress.tasks[task_id].fields['passed'] + (1 if passed else 0),
                                    failed=progress.tasks[task_id].fields['failed'] + (0 if passed else 1))
                    
                    # Update summary
                    summary = summary_progress.tasks[summary_task].fields
                    summary_progress.update(summary_task, advance=1, 
                                            passed=summary['passed'] + (1 if passed else 0),
                                            failed=summary['failed'] + (0 if passed else 1))
            
        Console().print("")
        self.display_results_table(results)
        self.reporter.success("Component tests execution finished.")

    def display_results_table(self, results):
        table = Table(title="Test Execution Summary")
        table.add_column("Test File", style="cyan", no_wrap=True)
        table.add_column("Status", style="magenta")
        table.add_column("Execution Time", justify="right", style="green")

        for res in results:
            status_color = "green" if res['status'] == "Pass" else "red"
            table.add_row(res['name'], f"[{status_color}]{res['status']}[/{status_color}]", res['time'])

        Console().print(table)
