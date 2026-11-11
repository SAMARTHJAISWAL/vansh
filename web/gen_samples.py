#!/usr/bin/env python3
"""Write web/samples.js-equivalent JavaScript to stdout from the test fixtures,
so the demo always shows exactly what the test suite checks."""
import json
import pathlib

root = pathlib.Path(__file__).resolve().parent.parent
models = sorted((root / "test/fixtures/dukaan/models").rglob("*.sql"))
dbt = [{"name": str(p.relative_to(root / "test/fixtures/dukaan")), "source": p.read_text()} for p in models]
# staging first, then marts: the order a reader expects
dbt.sort(key=lambda f: (0 if "/staging/" in f["name"] else 1, f["name"]))
samples = {
    "dbt": dbt,
    "query": [{"name": "order_attribution.sql", "source": (root / "web/sample_query.sql").read_text()}],
    "blank": [{"name": "query.sql", "source": "select\n    \nfrom "}],
}
print("window.VANSH_SAMPLES = " + json.dumps(samples, indent=1) + ";")
