#!/usr/bin/env python3
"""Interactive 3D viewer for the osu!mania accuracy surface over (OD, skill).

The surface converts judgement counts into an implied player skill, so how it
responds to OD and to window-widening mods *is* how mods get priced. This renders
that response as a browsable HTML file: one wireframe per judgement band, over an OD
axis, for one mod state at a time.

Everything is a wireframe rather than an opaque sheet on purpose. The bands overlap
heavily and a solid surface hides whatever sits beneath it; likewise only one mod
state is drawn at once, because EZ's surfaces lie directly above NM's and showing
both together just occludes the lower one.

Usage
-----
    # the default slice (13.77 stars, the Decoy DT score)
    tools/mania_surface.py

    # a real beatmap, at its own rated difficulty
    tools/mania_surface.py --map local-fixtures/maps/4055699.osu --clock-rate 1.5

    # an arbitrary difficulty, no beatmap needed
    tools/mania_surface.py --stars 8.5

    # reuse the last dump instead of re-running cargo
    tools/mania_surface.py --no-dump

Requires `plotly` and `numpy`. If they are missing:

    uv venv .venv && uv pip install --python .venv/bin/python plotly numpy
    .venv/bin/python tools/mania_surface.py

The data comes from the `od_surface_dump` test in `src/sunny.rs`, which this script
invokes via `cargo test`. Keep the CSV columns in sync with it.
"""

from __future__ import annotations

import argparse
import csv
import os
import subprocess
import sys
import webbrowser
from collections import defaultdict
from pathlib import Path

import numpy as np
import plotly.graph_objects as go

ROOT = Path(__file__).resolve().parent.parent
CSV = ROOT / "target" / "surface" / "od_grid.csv"
META = ROOT / "target" / "surface" / "meta.csv"

BANDS = [
    # csv column, label, colour, drawn by default
    ("n320", "320", "#ffd166", True),
    ("n300", "300", "#8fe3ff", True),
    ("n200", "200", "#7cffb2", True),
    ("n100", "100", "#c39bff", False),
    ("n50", "50", "#ff9f6b", False),
    ("miss", "miss", "#ff6b81", False),
]

# How dense the drawn lattice is, in grid points. The dump is finer than a wireframe
# can usefully show and every point is embedded verbatim in the HTML.
OD_STRIDE = 2
SKILL_STRIDE = 3
MESH_SKILL_EVERY = 4
MESH_OD_EVERY = 2

Z_AXES = {
    "bands": ("share of judgements", [0.0, 1.0]),
    "acc": ("305-weighted accuracy", [0.0, 1.0]),
    "gain": ("EZ gain in accuracy", None),
}


def dump(args: argparse.Namespace) -> None:
    """Re-run the Rust dump that produces the CSV."""
    env = dict(os.environ)

    if args.map:
        env["SURFACE_MAP"] = str(args.map)
    if args.stars is not None:
        env["SURFACE_STARS"] = str(args.stars)
    env["SURFACE_CLOCK_RATE"] = str(args.clock_rate)

    command = [
        "cargo", "test", "sunny::tests::od_surface_dump",
        "--", "--ignored", "--exact", "--nocapture",
    ]
    print("$", " ".join(command))
    result = subprocess.run(command, cwd=ROOT, env=env, check=False)

    if result.returncode != 0:
        sys.exit(f"cargo test failed with status {result.returncode}")


def load() -> tuple[dict, dict, list[float], list[float], str]:
    """Read the CSV into `[skill, od]` grids keyed by (scheme, mod, quantity)."""
    if not CSV.exists():
        sys.exit(f"{CSV} is missing; run without --no-dump to generate it")

    with CSV.open() as handle:
        rows = list(csv.DictReader(handle))

    ods = sorted({float(r["od"]) for r in rows})[::OD_STRIDE]
    skills = sorted({float(r["skill"]) for r in rows})[::SKILL_STRIDE]
    keep_od, keep_skill = set(ods), set(skills)
    rows = [
        r for r in rows
        if float(r["od"]) in keep_od and float(r["skill"]) in keep_skill
    ]

    oi = {value: index for index, value in enumerate(ods)}
    si = {value: index for index, value in enumerate(skills)}
    shape = (len(skills), len(ods))

    grid: dict = defaultdict(lambda: np.full(shape, np.nan))
    great: dict = defaultdict(lambda: np.full(len(ods), np.nan))

    for row in rows:
        key = (row["scheme"], row["mod"])
        y, x = si[float(row["skill"])], oi[float(row["od"])]

        for column, *_ in BANDS:
            # Rounded because the full f64 repr of every point ends up in the HTML.
            grid[(*key, column)][y, x] = round(float(row[column]), 5)

        grid[(*key, "accuracy")][y, x] = round(float(row["accuracy"]), 5)
        great[key][x] = float(row["great"])

    subtitle = ""

    if META.exists():
        with META.open() as handle:
            meta = next(csv.DictReader(handle), None)

        if meta:
            subtitle = (
                f"{float(meta['difficulty']):.2f} stars"
                f" · clock rate {float(meta['clock_rate']):g}"
                f" · {meta['source']}"
            )

    return grid, great, ods, skills, subtitle


def build(grid, great, ods, skills, subtitle: str) -> go.Figure:
    def lattice(z, extra):
        """Flatten a grid into one line trace: rows of constant skill, then columns
        of constant OD, separated by `None` breaks.

        `extra` rides along as customdata so hover can show a second quantity.
        """
        xs, ys, zs, cd = [], [], [], []

        for j in range(0, len(skills), MESH_SKILL_EVERY):
            xs.extend(ods + [None])
            ys.extend([skills[j]] * len(ods) + [None])
            zs.extend(list(z[j, :]) + [None])
            cd.extend(list(extra[j, :]) + [None])

        for i in range(0, len(ods), MESH_OD_EVERY):
            xs.extend([ods[i]] * len(skills) + [None])
            ys.extend(skills + [None])
            zs.extend(list(z[:, i]) + [None])
            cd.extend(list(extra[:, i]) + [None])

        return xs, ys, zs, cd

    traces: list[go.Scatter3d] = []
    meta: list[tuple[str, str, str, bool]] = []  # (view, scheme, mod, default_on)

    def add(*, view, scheme, mod, name, color, z, extra, hover, on, width=2.2):
        xs, ys, zs, cd = lattice(z, extra)
        traces.append(
            go.Scatter3d(
                x=xs, y=ys, z=zs,
                mode="lines",
                name=name,
                legendgroup=name,
                line=dict(color=color, width=width),
                customdata=cd,
                hovertemplate=hover,
                connectgaps=False,
                visible=False,
            )
        )
        meta.append((view, scheme, mod, on))

    shape = (len(skills), len(ods))

    for scheme in ("classic", "lazer"):
        for mod in ("NM", "EZ"):
            windows = np.broadcast_to(great[(scheme, mod)], shape)

            for column, label, color, on in BANDS:
                add(
                    view="bands", scheme=scheme, mod=mod,
                    name=label, color=color,
                    z=grid[(scheme, mod, column)], extra=windows,
                    hover=(
                        f"<b>{label} · {mod} · {scheme}</b><br>"
                        "OD %{x:.1f} (great %{customdata:.1f} ms)<br>"
                        "skill %{y:.2f}<br>share %{z:.4f}<extra></extra>"
                    ),
                    on=on,
                )

            add(
                view="acc", scheme=scheme, mod=mod,
                name="accuracy", color="#e6e8ee",
                z=grid[(scheme, mod, "accuracy")], extra=windows,
                hover=(
                    f"<b>accuracy · {mod} · {scheme}</b><br>"
                    "OD %{x:.1f} (great %{customdata:.1f} ms)<br>"
                    "skill %{y:.2f}<br>acc %{z:.4f}<extra></extra>"
                ),
                on=True,
            )

        # What EZ is worth, as a surface in its own right. A difference has no mod
        # state of its own; it is filed under "NM" so the masks stay a flat lookup.
        no_mod = grid[(scheme, "NM", "accuracy")]
        easy = grid[(scheme, "EZ", "accuracy")]

        add(
            view="gain", scheme=scheme, mod="NM",
            name="EZ gain", color="#ffd166",
            z=easy - no_mod, extra=no_mod,
            hover=(
                f"<b>EZ gain · {scheme}</b><br>"
                "OD %{x:.1f}<br>skill %{y:.2f}<br>"
                "+%{z:.4f} acc (from %{customdata:.4f})<extra></extra>"
            ),
            on=True, width=2.6,
        )

    def mask(view, scheme, mod):
        """Visibility list for one dropdown entry.

        `legendonly` rather than `False` for the off-by-default bands, so 100/50/miss
        stay listed in the legend and are one click from being drawn.
        """
        out = []

        for m_view, m_scheme, m_mod, m_on in meta:
            if (m_view, m_scheme, m_mod) != (view, scheme, mod):
                out.append(False)
            else:
                out.append(True if m_on else "legendonly")

        return out

    entries = [
        (f"{scheme} · {mod} · {view_label}", view, scheme, mod)
        for view, view_label in (("bands", "bands"), ("acc", "accuracy"))
        for scheme in ("classic", "lazer")
        for mod in ("NM", "EZ")
    ]
    entries += [
        (f"{scheme} · EZ gain", "gain", scheme, "NM")
        for scheme in ("classic", "lazer")
    ]

    buttons = []

    for label, view, scheme, mod in entries:
        title, zrange = Z_AXES[view]
        buttons.append(dict(
            label=label,
            method="update",
            args=[
                {"visible": mask(view, scheme, mod)},
                {"scene.zaxis.title.text": title, "scene.zaxis.range": zrange},
            ],
        ))

    figure = go.Figure(data=traces)

    # Open on the first entry.
    for trace, visible in zip(figure.data, buttons[0]["args"][0]["visible"]):
        trace.visible = visible

    axis = dict(backgroundcolor="#181b22", gridcolor="#2c313c", zerolinecolor="#3a4150")

    figure.update_layout(
        template="plotly_dark",
        paper_bgcolor="#12141a",
        title=dict(
            text=(
                "osu!mania accuracy surface over (OD, skill)"
                f"<br><sub>{subtitle}<br>"
                "one mod state at a time · click legend entries to add 100/50/miss"
                "</sub>"
            ),
            x=0.5, y=0.97,
        ),
        scene=dict(
            xaxis=dict(title="overall difficulty (OD)", **axis),
            yaxis=dict(title="player skill (star units)", type="log", **axis),
            zaxis=dict(title=Z_AXES["bands"][0], range=[0.0, 1.0], **axis),
            # Looking down the skill axis from the high-OD end: the OD tilt is the
            # point of the third axis and it is edge-on from the default corner.
            camera=dict(eye=dict(x=-1.55, y=-1.85, z=1.0)),
            aspectratio=dict(x=1.0, y=1.35, z=0.7),
        ),
        updatemenus=[dict(
            type="dropdown", direction="down", x=0.01, xanchor="left", y=1.0,
            buttons=buttons, bgcolor="#181b22", bordercolor="#2c313c",
            font=dict(size=11), active=0, showactive=True,
        )],
        legend=dict(
            x=0.01, y=0.72, bgcolor="rgba(24,27,34,0.7)",
            bordercolor="#2c313c", borderwidth=1,
        ),
        margin=dict(l=0, r=0, t=110, b=0),
        height=900,
    )

    return figure


def main() -> None:
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument(
        "--map", type=Path,
        help="beatmap to take the difficulty slice from (.osu path)",
    )
    parser.add_argument(
        "--stars", type=float,
        help="difficulty to slice at, if no beatmap is given",
    )
    parser.add_argument(
        "--clock-rate", type=float, default=1.0,
        help="clock rate, e.g. 1.5 for DT (default 1.0)",
    )
    parser.add_argument(
        "--out", type=Path, default=ROOT / "target" / "surface" / "mania_surface.html",
        help="where to write the HTML",
    )
    parser.add_argument(
        "--no-dump", action="store_true",
        help="reuse the existing CSV instead of re-running cargo",
    )
    parser.add_argument(
        "--open", action="store_true",
        help="open the result in a browser when done",
    )
    args = parser.parse_args()

    if args.map and args.stars is not None:
        parser.error("--map and --stars are mutually exclusive")

    if not args.no_dump:
        dump(args)

    figure = build(*load())
    args.out.parent.mkdir(parents=True, exist_ok=True)
    # CDN rather than inline plotly.js: keeps the file at a couple of MB instead of
    # ~7 MB, at the cost of needing a network connection to open it.
    figure.write_html(args.out, include_plotlyjs="cdn")

    size = args.out.stat().st_size / 1e6
    print(f"wrote {args.out} ({size:.1f} MB)")

    if args.open:
        webbrowser.open(args.out.as_uri())


if __name__ == "__main__":
    main()
