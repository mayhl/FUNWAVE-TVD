from rich.console import Console

class ConsoleReporter:
    def __init__(self):
        self.console = Console()

    def info(self, message):
        self.console.print(f"[blue]INFO:[/blue] {message}")

    def success(self, message):
        self.console.print(f"[green]SUCCESS:[/green] {message}")

    def error(self, message):
        self.console.print(f"[red]ERROR:[/red] {message}")

    def step(self, message):
        self.console.print(f"[bold yellow]-- {message}[/bold yellow]")
