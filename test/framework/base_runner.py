from abc import ABC, abstractmethod


class BaseRunner(ABC):
    """A test tier: reports through `reporter`, runs via run()."""

    def __init__(self, reporter):
        self.reporter = reporter

    @abstractmethod
    def run(self):
        """Execute the test logic for this tier."""
        pass
