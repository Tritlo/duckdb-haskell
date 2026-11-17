#!/usr/bin/env python3
"""Alternate checked baseline and candidate processes for each fixture shape."""

import csv
import hashlib
import json
import os
import pathlib
import subprocess
import sys

baseline, candidate, output = map(pathlib.Path, sys.argv[1:])
output.mkdir(parents=True, exist_ok=True)
executables = {"baseline": baseline, "candidate": candidate}
metadata = {"load_before": os.getloadavg(), "order": [], "executable_sha256": {}}
for label, executable in executables.items():
    metadata["executable_sha256"][label] = hashlib.sha256(executable.read_bytes()).hexdigest()
with (output / "comparison.csv").open("w", newline="") as samples:
    writer = None
    for round_number in range(1, 8):
        labels = ["baseline", "candidate"] if round_number % 2 else ["candidate", "baseline"]
        for shape in ["scalar", "array", "object", "unusual", "mixed"]:
            for label in labels:
                result = subprocess.check_output(
                    [str(executables[label]), shape, "10000", "1", "+RTS", "-N1", "-T"],
                    text=True,
                )
                for row in csv.DictReader(result.splitlines()):
                    sample = {"implementation": label, "round": round_number, **row}
                    if writer is None:
                        writer = csv.DictWriter(samples, fieldnames=sample, lineterminator="\n")
                        writer.writeheader()
                    writer.writerow(sample)
                    samples.flush()
                metadata["order"].append([round_number, shape, label])
metadata["load_after"] = os.getloadavg()
(output / "comparison.json").write_text(json.dumps(metadata, indent=2) + "\n")
