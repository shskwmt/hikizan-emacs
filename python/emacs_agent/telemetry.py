import logging
from typing import Sequence

from opentelemetry.sdk.trace import ReadableSpan
from opentelemetry.sdk.trace.export import (
    BatchSpanProcessor,
    SpanExporter,
    SpanExportResult,
)
from google.adk.telemetry.setup import maybe_set_otel_providers, OTelHooks

logger = logging.getLogger(__name__)


class LocalFileSpanExporter(SpanExporter):
    """Exporter that writes spans to a local file in JSONL format."""

    def __init__(self, file_path: str):
        self.file_path = file_path

    def export(self, spans: Sequence[ReadableSpan]) -> SpanExportResult:
        try:
            with open(self.file_path, "a", encoding="utf-8") as f:
                for span in spans:
                    f.write(span.to_json(indent=None) + "\n")
            return SpanExportResult.SUCCESS
        except Exception as e:
            logger.error(f"Failed to export spans to {self.file_path}: {e}")
            return SpanExportResult.FAILURE

    def shutdown(self) -> None:
        pass


def initialize_telemetry(output_path: str = "telemetry_output.json") -> None:
    """Initializes OpenTelemetry with a local file exporter."""
    exporter = LocalFileSpanExporter(output_path)
    # Using BatchSpanProcessor for efficiency as per technical requirements
    span_processor = BatchSpanProcessor(exporter)

    hooks = OTelHooks(span_processors=[span_processor])
    maybe_set_otel_providers(otel_hooks_to_setup=[hooks])
    logger.info(f"Telemetry initialized. Traces will be saved to {output_path}")


def instrument(output_path: str = "telemetry_output.json") -> None:
    """Alias for initialize_telemetry to match AI Agent SDK requirements."""
    initialize_telemetry(output_path)
