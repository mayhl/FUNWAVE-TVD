from rich.console import Console

class ConsoleReporter:
    def __init__(self):
        self.console = Console()

    def info(self, msg):
        self.console.print(f"[blue]INFO:[/blue] {msg}")

    def warn(self, msg):
        self.console.print(f"[yellow]WARN:[/yellow] {msg}")

    def success(self, msg):
        self.console.print(f"[bold green]PASS:[/bold green] {msg}")

    def error(self, message):
        self.console.print(f"[bold red]FAIL:[/bold red] {message}")

    def step(self, message):
        self.console.print(f"[bold yellow]-- {message}[/bold yellow]")
