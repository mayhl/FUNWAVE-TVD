from rich.console import Console


class ConsoleReporter:
    """Rich console lines by severity."""

    def __init__(self):
        self.console = Console()

    def info(self, msg):
        """Blue INFO line."""
        self.console.print(f"[blue]INFO:[/blue] {msg}")

    def warn(self, msg):
        """Yellow WARN line."""
        self.console.print(f"[yellow]WARN:[/yellow] {msg}")

    def success(self, msg):
        """Green PASS line."""
        self.console.print(f"[bold green]PASS:[/bold green] {msg}")

    def error(self, message):
        """Red FAIL line."""
        self.console.print(f"[bold red]FAIL:[/bold red] {message}")

    def step(self, message):
        """Yellow section header."""
        self.console.print(f"[bold yellow]-- {message}[/bold yellow]")
