#!/usr/bin/env python3
"""Static four-panel overview of the osu!mania accuracy surface.

The companion to `mania_surface.py`. That one is browsable and varies OD; this one
is a single PNG at fixed windows, and is the better starting point because it shows
the whole (difficulty, skill) plane at once rather than one difficulty slice.

Panels:

1. Accuracy shortfall `1 - acc` over (difficulty, skill), log-scaled. Plotting the
   shortfall rather than accuracy matters: accuracy is flat above ~95% over most of
   the plane and hides all the structure. Straight contours here are the
   `skill_exponent` at work.
2. Judgement composition against skill at one difficulty, the mechanism the surface
   integrates.
3. The same difficulty under several window sets. Curves are near-parallel, so
   `window_scalar` reads as the horizontal gap between them.
4. Miss rate over the plane. There is no separate miss term; misses are just the
   mass beyond the last window.

Usage
-----
    tools/mania_surface_2d.py
    tools/mania_surface_2d.py --map path/to.osu --out /tmp/surface.png

Requires `matplotlib` and `numpy`. Data comes from the `surface_dump` test in
`src/sunny.rs`, which this script invokes via `cargo test`.
"""

from __future__ import annotations

import argparse
import csv
import os
import subprocess
import sys
from collections import defaultdict
from pathlib import Path

import matplotlib

matplotlib.use("Agg")

import matplotlib.pyplot as plt  # noqa: E402
import numpy as np  # noqa: E402

ROOT = Path(__file__).resolve().parent.parent
DATA = ROOT / "target" / "surface"

BANDS = [
    ("n320", "320", "#ffd166"),
    ("n300", "300", "#8fe3ff"),
    ("n200", "200", "#7cffb2"),
    ("n100", "100", "#c39bff"),
    ("n50", "50", "#ff9f6b"),
    ("miss", "miss", "#ff6b81"),
]

WINDOW_COLORS = {
    "HR": "#ff6b81",
    "natural": "#e6e8ee",
    "NM": "#8fe3ff",
    "EZ": "#7cffb2",
    "HR OD7 DT": "#ff6b81",
    "reference OD8": "#e6e8ee",
    "OD7 DT": "#8fe3ff",
    "EZ OD7 DT": "#7cffb2",
}

FG = "#f2f4f8"
DIM = "#aab0bd"
TICK = "#8d94a3"
PANEL = "#181b22"
PAPER = "#12141a"
EDGE = "#2c313c"


def read(name: str) -> list[dict]:
    path = DATA / name

    if not path.exists():
        sys.exit(f"{path} is missing; run without --no-dump to generate it")

    with path.open() as handle:
        return list(csv.DictReader(handle))


def dump(args: argparse.Namespace) -> None:
    # `--exact`: the bare name also substring-matches `od_surface_dump`, which would
    # clobber that tool's CSV with a default-parameter dump as a side effect.
    command = [
        "cargo", "test", "sunny::tests::surface_dump",
        "--", "--ignored", "--exact", "--nocapture",
    ]
    print("$", " ".join(command))
    env = dict(os.environ)
    if args.map:
        env["SURFACE_MAP"] = str(args.map)
    env["SURFACE_CLOCK_RATE"] = str(args.clock_rate)
    result = subprocess.run(command, cwd=ROOT, env=env, check=False)

    if result.returncode != 0:
        sys.exit(f"cargo test failed with status {result.returncode}")


def style(ax, title: str, xlabel: str, ylabel: str) -> None:
    ax.set_facecolor(PANEL)
    ax.set_title(title, color=FG, fontsize=12, pad=10)
    ax.set_xlabel(xlabel, color=DIM, fontsize=10)
    ax.set_ylabel(ylabel, color=DIM, fontsize=10)
    ax.tick_params(colors=TICK, labelsize=9)

    for spine in ax.spines.values():
        spine.set_color(EDGE)

    ax.grid(alpha=0.12, color="#7d8494", lw=0.6)


def plain_log(ax, which: str, ticks: list[float]) -> None:
    """Log axis with readable labels: matplotlib's default minor labels overlap."""
    axis = ax.get_xaxis() if which == "x" else ax.get_yaxis()
    (ax.set_xticks if which == "x" else ax.set_yticks)(ticks)
    axis.set_major_formatter(matplotlib.ticker.ScalarFormatter())
    axis.set_minor_formatter(matplotlib.ticker.NullFormatter())


def legend(ax, **kwargs):
    handle = ax.legend(facecolor=PANEL, edgecolor=EDGE, fontsize=8, **kwargs)

    for text in handle.get_texts():
        text.set_color("#cdd3de")

    return handle


def colorbar(figure, mappable, ax, label: str) -> None:
    bar = figure.colorbar(mappable, ax=ax, pad=0.02)
    bar.set_label(label, color=DIM, fontsize=9)
    bar.ax.tick_params(colors=TICK, labelsize=8)
    bar.outline.set_edgecolor(EDGE)


def load_grid() -> tuple[np.ndarray, np.ndarray, list[float], list[float]]:
    rows = read("grid.csv")
    diffs = sorted({float(r["difficulty"]) for r in rows})
    skills = sorted({float(r["skill"]) for r in rows})
    di = {v: i for i, v in enumerate(diffs)}
    si = {v: i for i, v in enumerate(skills)}

    acc = np.zeros((len(skills), len(diffs)))
    miss = np.zeros_like(acc)

    for row in rows:
        y, x = si[float(row["skill"])], di[float(row["difficulty"])]
        acc[y, x] = float(row["accuracy"])
        miss[y, x] = float(row["miss_rate"])

    return acc, miss, diffs, skills


DIFF_TICKS = [2, 3, 5, 8, 12, 20]
SKILL_TICKS = [0.5, 1, 2, 5, 10, 20, 40]


def panel_shortfall(figure, ax, acc, diffs, skills, X, Y) -> None:
    style(
        ax,
        "accuracy shortfall (1 - acc) over (difficulty, skill)\n"
        "at reference windows (OD8 classic)",
        "map difficulty (stars)",
        "player skill (star units)",
    )

    # Accuracy itself is flat over most of the plane; the shortfall on a log scale is
    # where the structure lives.
    shortfall = np.clip(1.0 - acc, 1e-6, 1.0)
    filled = ax.contourf(
        X, Y, np.log10(shortfall), levels=np.linspace(-6, 0, 25), cmap="magma"
    )
    lines = ax.contour(
        X, Y, acc,
        levels=[0.50, 0.80, 0.90, 0.95, 0.99, 0.999],
        colors="#8fe3ff", linewidths=0.9, alpha=0.9,
    )
    ax.clabel(lines, fmt=lambda v: f"{v * 100:g}%", fontsize=8, colors="#cfefff")

    ax.set_xscale("log")
    ax.set_yscale("log")
    plain_log(ax, "x", DIFF_TICKS)
    plain_log(ax, "y", SKILL_TICKS)

    ax.plot(diffs, diffs, color="#ffffff", ls="--", lw=1.0, alpha=0.5,
            label="skill = difficulty")
    ax.plot(diffs, [3.7 * d for d in diffs], color="#ff9f6b", ls=":", lw=1.2,
            alpha=0.85, label="saturation (3.7x)")
    ax.set_ylim(min(skills), max(skills))
    legend(ax, loc="lower right")
    colorbar(figure, filled, ax, "log10 (1 - accuracy)")


def panel_bands(ax, difficulty: float, fit_skill: float | None) -> None:
    rows = read("bands.csv")
    skills = np.array([float(r["skill"]) for r in rows])
    sigma = np.array([float(r["sigma"]) for r in rows])
    shares = {col: np.array([float(r[col]) for r in rows]) for col, *_ in BANDS}

    style(
        ax,
        f"judgement composition vs skill at {difficulty:.2f} stars\n"
        "(the mechanism the surface integrates)",
        "player skill (star units)",
        "share of judgements",
    )
    ax.stackplot(
        skills,
        [shares[col] for col, *_ in BANDS],
        labels=[label for _, label, _ in BANDS],
        colors=[color for *_, color in BANDS],
        alpha=0.92,
    )

    ax.set_xscale("log")
    ax.set_xlim(skills.min(), skills.max())
    ax.set_ylim(0, 1)
    plain_log(ax, "x", SKILL_TICKS)

    if fit_skill:
        ax.axvline(fit_skill, color="#ffffff", ls="--", lw=1.0, alpha=0.6)

    legend(ax, loc="center left", ncol=2)

    # sigma is what actually varies; the bands are just it read through the windows.
    twin = ax.twinx()
    twin.plot(skills, sigma, color="#ffffff", lw=1.2, alpha=0.45)
    twin.set_yscale("log")
    twin.set_ylabel("implied sigma (ms)", color=TICK, fontsize=9)
    twin.tick_params(colors="#6f7686", labelsize=8)

    for spine in twin.spines.values():
        spine.set_color(EDGE)


def panel_windows(ax, target: float | None) -> None:
    rows = read("windows.csv")
    series: dict[str, tuple[list[float], list[float]]] = defaultdict(lambda: ([], []))
    greats: dict[str, float] = {}

    for row in rows:
        label = row["label"]
        greats[label] = float(row["great"])
        series[label][0].append(float(row["skill"]))
        series[label][1].append(float(row["accuracy"]))

    style(
        ax,
        "same difficulty, different windows\n(the horizontal gap is window_scalar)",
        "player skill (star units)",
        "305-weighted accuracy",
    )

    for label, (xs, ys) in series.items():
        ax.plot(xs, ys, lw=1.8, color=WINDOW_COLORS.get(label, "#aaaaaa"),
                label=f"{label}  (great {greats[label]:.1f} ms)")

    ax.set_xscale("log")
    ax.set_xlim(1, 60)
    ax.set_ylim(0.4, 1.005)
    plain_log(ax, "x", [1, 2, 5, 10, 20, 40])
    legend(ax, loc="lower right")

    # A horizontal read at one accuracy is exactly what `window_scalar` computes.
    if target:
        ax.axhline(target, color="#ffd166", ls="--", lw=1.0, alpha=0.7)
        ax.annotate(f"{target * 100:.2f}%", (1.15, target + 0.008),
                    color="#ffd166", fontsize=8)


def panel_misses(figure, ax, miss, diffs, skills, X, Y) -> None:
    style(
        ax,
        "miss rate over (difficulty, skill)\n"
        "misses come from the timing tail, not a separate term",
        "map difficulty (stars)",
        "player skill (star units)",
    )

    rate = np.clip(miss, 1e-6, 1.0)
    filled = ax.contourf(
        X, Y, np.log10(rate), levels=np.linspace(-6, 0, 25), cmap="inferno"
    )
    lines = ax.contour(X, Y, np.log10(rate), levels=[-4, -3, -2, -1],
                       colors="#8fe3ff", linewidths=0.9, alpha=0.85)
    ax.clabel(lines, fmt=lambda v: f"{10 ** v:.2%}", fontsize=8, colors="#cfefff")

    ax.set_xscale("log")
    ax.set_yscale("log")
    plain_log(ax, "x", DIFF_TICKS)
    plain_log(ax, "y", SKILL_TICKS)
    ax.plot(diffs, diffs, color="#ffffff", ls="--", lw=1.0, alpha=0.5)
    ax.set_ylim(min(skills), max(skills))
    colorbar(figure, filled, ax, "log10 miss rate")


def main() -> None:
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument("--out", type=Path, default=DATA / "mania_surface_2d.png")
    parser.add_argument("--map", type=Path,
                        help="real beatmap supplying difficulty, windows, and note units")
    parser.add_argument("--clock-rate", type=float, default=1.0,
                        help="clock rate used to rate the map (default 1.0)")
    parser.add_argument("--fit-skill", type=float, default=None,
                        help="mark this skill on the composition panel")
    parser.add_argument("--target-accuracy", type=float, default=None,
                        help="mark this accuracy on the windows panel")
    parser.add_argument("--no-dump", action="store_true",
                        help="reuse the existing CSVs instead of re-running cargo")
    args = parser.parse_args()

    if not args.no_dump:
        dump(args)

    difficulty = 13.774
    source = "default synthetic slice"
    meta_path = DATA / "surface_2d_meta.csv"
    if meta_path.exists():
        with meta_path.open() as handle:
            meta = next(csv.DictReader(handle), None)
        if meta:
            difficulty = float(meta["difficulty"])
            source = Path(meta["source"]).stem

    acc, miss, diffs, skills = load_grid()
    X, Y = np.meshgrid(diffs, skills)

    figure = plt.figure(figsize=(16, 11), facecolor=PAPER)
    grid = figure.add_gridspec(2, 2, hspace=0.30, wspace=0.24)

    panel_shortfall(figure, figure.add_subplot(grid[0, 0]), acc, diffs, skills, X, Y)
    panel_bands(figure.add_subplot(grid[0, 1]), difficulty, args.fit_skill)
    panel_windows(figure.add_subplot(grid[1, 0]), args.target_accuracy)
    panel_misses(figure, figure.add_subplot(grid[1, 1]), miss, diffs, skills, X, Y)

    figure.suptitle(
        f"osu!mania hit result surface  —  {source}  —  {difficulty:.2f} stars",
        color=FG, fontsize=14, y=0.965,
    )

    args.out.parent.mkdir(parents=True, exist_ok=True)
    figure.savefig(args.out, dpi=130, facecolor=PAPER, bbox_inches="tight")
    print(f"wrote {args.out}")


if __name__ == "__main__":
    main()
