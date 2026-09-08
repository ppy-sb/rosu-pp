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

Requires `matplotlib` and `numpy`. Data comes from the `surface_dump` binary,
which this script invokes via `cargo run`.
"""

from __future__ import annotations

import argparse
import csv
import os
import subprocess
import sys
import urllib.error
import urllib.request
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
    global DATA
    DATA = args.data_dir
    command = [
        "cargo",
        "run",
        "--release",
        "--bin",
        "surface_dump",
        "--features",
        "reports",
        "--",
        "--data-dir",
        str(args.data_dir),
        "--clock-rate",
        str(args.clock_rate),
        "--sigmas",
        ",".join(str(x) for x in SIGMA_TICKS),
    ]
    if args.map:
        command.extend(["--map", str(args.map)])
    if args.fit_sigma is not None:
        command.extend(["--core-sigma", str(args.fit_sigma)])
    print("$", " ".join(command))
    result = subprocess.run(command, cwd=ROOT, check=False)

    if result.returncode != 0:
        sys.exit(f"cargo run failed with status {result.returncode}")


def fetch_map(path: Path) -> Path:
    """Fetch a beatmap ID into the repository's local fixture map directory."""
    map_id = path.stem if path.suffix else path.name
    if not map_id.isdigit():
        sys.exit(f"--fetch requires a numeric beatmap ID or path, got {path}")

    destination = ROOT / "local-fixtures" / "maps" / f"{map_id}.osu"
    if destination.exists():
        return destination

    destination.parent.mkdir(parents=True, exist_ok=True)
    url = f"https://osu.ppy.sh/osu/{map_id}"
    try:
        with urllib.request.urlopen(url, timeout=30) as response:
            destination.write_bytes(response.read())
    except (urllib.error.URLError, OSError) as error:
        destination.unlink(missing_ok=True)
        sys.exit(f"failed to fetch beatmap {map_id} from {url}: {error}")

    print(f"fetched beatmap {map_id} -> {destination}")
    return destination


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
    sigmas = sorted({float(r["sigma"]) for r in rows}, reverse=True)  # Reverse so lower sigma (better) is higher
    di = {v: i for i, v in enumerate(diffs)}
    si = {v: i for i, v in enumerate(sigmas)}

    acc = np.zeros((len(sigmas), len(diffs)))
    miss = np.zeros_like(acc)

    for row in rows:
        y, x = si[float(row["sigma"])], di[float(row["difficulty"])]
        acc[y, x] = float(row["accuracy"])
        miss[y, x] = float(row["miss_rate"])

    return acc, miss, diffs, sigmas


DIFF_TICKS = [2, 3, 5, 8, 12, 20]
SIGMA_TICKS = [2, 4, 6, 8, 10, 12, 16, 24, 32]  # Timing spread in milliseconds


def panel_shortfall(figure, ax, acc, diffs, sigmas, X, Y) -> None:
    style(
        ax,
        "accuracy shortfall (1 - acc) over (difficulty, sigma)\n" "at reference windows (OD8 classic)",
        "map difficulty (stars)",
        "timing spread sigma (ms)",
    )

    # Accuracy itself is flat over most of the plane; the shortfall on a log scale is
    # where the structure lives.
    shortfall = np.clip(1.0 - acc, 1e-6, 1.0)
    filled = ax.contourf(X, Y, np.log10(shortfall), levels=np.linspace(-6, 0, 25), cmap="magma")
    lines = ax.contour(
        X,
        Y,
        acc,
        levels=[0.50, 0.80, 0.90, 0.95, 0.99, 0.999],
        colors="#8fe3ff",
        linewidths=0.9,
        alpha=0.9,
    )
    ax.clabel(lines, fmt=lambda v: f"{v * 100:g}%", fontsize=8, colors="#cfefff")

    ax.set_xscale("log")
    ax.set_yscale("log")
    plain_log(ax, "x", diffs)
    plain_log(ax, "y", sigmas)

    ax.plot(diffs, diffs, color="#ffffff", ls="--", lw=1.0, alpha=0.5, label="sigma = difficulty")
    ax.set_ylim(min(sigmas), max(sigmas))
    legend(ax, loc="lower right")
    colorbar(figure, filled, ax, "log10 (1 - accuracy)")


def panel_bands(ax, difficulty: float, fit_sigma: float | None) -> None:
    rows = read("bands.csv")
    sigmas = np.array([float(r["sigma"]) for r in rows])
    shares = {col: np.array([float(r[col]) for r in rows]) for col, *_ in BANDS}

    style(
        ax,
        f"judgement composition vs timing spread at {difficulty:.2f} stars\n"
        "(the mechanism the surface integrates)",
        "timing spread sigma (ms)",
        "share of judgements",
    )
    ax.stackplot(
        sigmas,
        [shares[col] for col, *_ in BANDS],
        labels=[label for _, label, _ in BANDS],
        colors=[color for *_, color in BANDS],
        alpha=0.92,
    )

    ax.set_xscale("log")
    ax.set_xlim(sigmas.min(), sigmas.max())
    ax.set_ylim(0, 1)
    plain_log(ax, "x", sigmas)

    if fit_sigma:
        ax.axvline(fit_sigma, color="#ffffff", ls="--", lw=1.0, alpha=0.6)

    legend(ax, loc="center left", ncol=2)


def panel_windows(ax, target: float | None) -> None:
    rows = read("windows.csv")
    series: dict[str, tuple[list[float], list[float]]] = defaultdict(lambda: ([], []))
    greats: dict[str, float] = {}

    for row in rows:
        label = row["label"]
        greats[label] = float(row["great"])
        series[label][0].append(float(row["sigma"]))
        series[label][1].append(float(row["accuracy"]))

    # Get actual sigma range from data
    all_sigmas = sorted({s for xs, _ in series.values() for s in xs})

    style(
        ax,
        "same difficulty, different windows\n(the horizontal gap is window_scalar)",
        "timing spread sigma (ms)",
        "305-weighted accuracy",
    )

    for label, (xs, ys) in series.items():
        ax.plot(
            xs,
            ys,
            lw=1.8,
            color=WINDOW_COLORS.get(label, "#aaaaaa"),
            label=f"{label}  (great {greats[label]:.1f} ms)",
        )

    ax.set_xscale("log")
    ax.set_xlim(min(all_sigmas), max(all_sigmas))
    ax.set_ylim(0.4, 1.005)
    plain_log(ax, "x", all_sigmas)
    legend(ax, loc="lower left")

    # A horizontal read at one accuracy is exactly what `window_scalar` computes.
    if target:
        ax.axhline(target, color="#ffd166", ls="--", lw=1.0, alpha=0.7)
        ax.annotate(f"{target * 100:.2f}%", (min(all_sigmas) * 1.1, target + 0.008), color="#ffd166", fontsize=8)


def panel_input_state(ax) -> None:
    expected_path = DATA / "per_note_expected_counts.csv"
    if expected_path.exists():
        rows = read("per_note_expected_counts.csv")
        base = [r for r in rows if r["variant"] == "baseline"]
        t = np.array([float(r["time_ms"]) / 1000.0 for r in base])
        colors = {
            "miss": "#ef476f",
            "p50": "#ff9f6b",
            "p100": "#c39bff",
            "p200": "#7cffb2",
            "p300": "#8fe3ff",
            "p320": "#ffd166",
        }
        bottom = np.zeros(len(base))
        for key in ("miss", "p50", "p100", "p200", "p300", "p320"):
            values = np.array([float(r[key]) * float(r["difficulty"]) for r in base])
            ax.fill_between(t, bottom, bottom + values, step="mid", color=colors[key], alpha=0.78, label=key)
            bottom += values
        ax.plot(t, bottom, color="#f4f7fa", lw=0.7, alpha=0.6)
        style(
            ax,
            "Sunny d-weighted expected hit-result composition",
            "map time (s)",
            "d × expected judgement share",
        )
        ax.set_xlabel("")
        ax.set_ylim(bottom=0)
        legend(ax, loc="upper right", ncol=2)
        return
    if (DATA / "per_note_difficulty.csv").exists():
        rows = read("per_note_difficulty.csv")
        t = np.array([float(r["time_ms"]) / 1000.0 for r in rows])
        d = np.array([float(r["difficulty"]) for r in rows])
        hold = np.array([float(r["hold_duration_ms"]) for r in rows])
        is_ln = hold > 0
        rice = np.where(~is_ln, d, np.nan)
        ln = np.where(is_ln, d, np.nan)
        # Narrow bars preserve every note's contribution; the line gives the
        # combined acc_d driver without smoothing away local changes.
        width = max(np.nanmedian(np.diff(t)) * 0.72, 0.008) if len(t) > 1 else 0.02
        ax.bar(
            t,
            np.where(~is_ln, d, 0.0),
            width=width,
            color="#ff9f6b",
            alpha=0.72,
            linewidth=0,
            label="rice d_all",
        )
        ax.bar(
            t,
            np.where(is_ln, d, 0.0),
            width=width,
            color="#7cffb2",
            alpha=0.72,
            linewidth=0,
            label="LN d_all",
        )
        ax.plot(t, d, color="#8fe3ff", lw=0.8, alpha=0.9, label="all notes d_all")
        ax.set_ylim(bottom=0)

        # A compact occupancy strip keeps note-type changes aligned with the
        # difficulty trace without hiding the actual per-note values.
        strip = ax.inset_axes([0.0, 0.0, 1.0, 0.13], sharex=ax)
        strip.fill_between(t, 0, (~is_ln).astype(float), step="mid", color="#ff9f6b", alpha=0.55)
        strip.fill_between(t, 1, 1 + is_ln.astype(float), step="mid", color="#7cffb2", alpha=0.55)
        strip.set_ylim(0, 2)
        strip.set_yticks([])
        strip.set_facecolor("none")
        strip.spines[:].set_visible(False)
        strip.set_xlabel("note occupancy: rice / LN", fontsize=8, color="#b9c8d6", labelpad=1)
        style(ax, "per-note accuracy-driver overlay", "map time (s)", "d_all / acc_d driver")
        legend(ax, loc="upper right")
        return
    rows = read("input_state_bins.csv")
    # Bins are difficulty-quantiled within each input class. Plot their means in
    # exported order as the map-level d(t) trace available from the harness.
    x = np.arange(len(rows))
    y = np.array([float(r["mean_difficulty"]) for r in rows])
    colors = {
        name: color
        for name, color in zip(
            sorted({r["class"] for r in rows}),
            ["#8fe3ff", "#7cffb2", "#ff9f6b", "#c39bff", "#ff6b81", "#ffd166", "#e6e8ee"],
        )
    }
    ax.plot(x, y, color="#8fe3ff", lw=1.2, alpha=0.8)
    for i, row in enumerate(rows):
        ax.scatter(i, y[i], s=14, color=colors.get(row["class"], "#aaaaaa"), zorder=3)
    style(
        ax,
        "per-note difficulty over map progression",
        "map progression (exported state bins)",
        "d_all / acc_d driver",
    )
    ax.set_yscale("log")


def panel_probability_balance(ax) -> None:
    """Show 320 probability above zero and all lower results below it."""
    expected_path = DATA / "per_note_expected_counts.csv"
    if not expected_path.exists():
        ax.set_visible(False)
        return

    rows = read("per_note_expected_counts.csv")
    rows = [r for r in rows if r["variant"] == "baseline"]
    t = np.array([float(r["time_ms"]) / 1000.0 for r in rows])
    colors = {
        "miss": "#ef476f",
        "p50": "#ff9f6b",
        "p100": "#c39bff",
        "p200": "#7cffb2",
        "p300": "#8fe3ff",
        "p320": "#ffd166",
    }

    # The zero line is the visual divider. 320 is the fixed reference level;
    # lower judgements are shown as ratios against each note's 320 probability
    # so the panel describes composition rather than raw probability.
    # 320 is the fixed reference level for this comparison, not a varying
    # series; keep it anchored at -1 so the lower outcomes remain readable.
    ax.fill_between(t, -1.0, 0.0, step="mid", color=colors["p320"], alpha=0.1, label="320 reference")
    p320 = np.array([float(r["p320"]) for r in rows])
    lower = ("miss", "p50", "p100", "p200", "p300")
    for key in lower:
        probability = np.array([float(r[key]) for r in rows])
        relative_probability = np.divide(
            probability,
            p320,
            out=np.zeros_like(probability),
            where=p320 > 0.0,
        )
        next_stack = -relative_probability
        ax.fill_between(t, 0.0, next_stack, step="mid", color=colors[key], alpha=0.25, label=key)
        ax.plot(t, next_stack, color=colors[key], lw=0.75, alpha=1.0)

    style(ax, "", "map time (s)", "probability vs 320 (320 = -1)")
    ax.set_ylim(min(-1.05, float(np.nanmin(next_stack)) - 0.03), 0.05)
    legend(ax, loc="upper center", bbox_to_anchor=(0.5, -0.34), ncol=6, borderaxespad=0.0)


def panel_misses(figure, ax, miss, diffs, sigmas, X, Y) -> None:
    style(
        ax,
        "miss rate over (difficulty, sigma)\n" "misses come from the timing tail, not a separate term",
        "map difficulty (stars)",
        "timing spread sigma (ms)",
    )

    rate = np.clip(miss, 1e-6, 1.0)
    filled = ax.contourf(X, Y, np.log10(rate), levels=np.linspace(-6, 0, 25), cmap="inferno")
    lines = ax.contour(
        X, Y, np.log10(rate), levels=[-4, -3, -2, -1], colors="#8fe3ff", linewidths=0.9, alpha=0.85
    )
    ax.clabel(lines, fmt=lambda v: f"{10 ** v:.2%}", fontsize=8, colors="#cfefff")

    ax.set_xscale("log")
    ax.set_yscale("log")
    plain_log(ax, "x", diffs)
    plain_log(ax, "y", sigmas)
    ax.plot(diffs, diffs, color="#ffffff", ls="--", lw=1.0, alpha=0.5)
    ax.set_ylim(min(sigmas), max(sigmas))
    colorbar(figure, filled, ax, "log10 miss rate")


def main() -> None:
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument(
        "--data-dir",
        type=Path,
        default=Path(os.environ.get("TMPDIR", "/tmp")) / "rosu-pp-surface",
        help="directory for temporary CSV data (default: a directory under TMPDIR)",
    )
    parser.add_argument(
        "--map",
        type=Path,
        required=True,
        help="beatmap supplying difficulty, windows, and note units",
    )
    parser.add_argument(
        "--out",
        type=Path,
        default=Path(os.environ.get("TMPDIR", "/tmp")) / "mania_surface_2d.png",
        help="PNG output path",
    )
    parser.add_argument(
        "--fetch",
        action="store_true",
        help="download a missing numeric beatmap ID into local-fixtures/maps",
    )
    parser.add_argument(
        "--clock-rate", type=float, default=1.0, help="clock rate used to rate the map (default 1.0)"
    )
    parser.add_argument(
        "--fit-sigma",
        type=float,
        default=None,
        help="use this core timing spread (ms) for the per-note overlay and mark it on the bands panel",
    )
    parser.add_argument(
        "--target-accuracy", type=float, default=None, help="mark this accuracy on the windows panel"
    )
    parser.add_argument(
        "--no-dump", action="store_true", help="reuse the existing CSVs instead of re-running cargo"
    )
    args = parser.parse_args()

    if args.fit_sigma is not None and (not np.isfinite(args.fit_sigma) or args.fit_sigma <= 0.0):
        parser.error("--fit-sigma must be a finite, positive number")

    args.data_dir = args.data_dir.resolve()
    DATA = args.data_dir

    if args.fetch:
        args.map = fetch_map(args.map)

    if not args.no_dump:
        dump(args)

    difficulty = 0.0
    source = args.map.stem
    meta_path = DATA / "surface_2d_meta.csv"
    dumped_core_sigma = None
    if meta_path.exists():
        with meta_path.open() as handle:
            meta = next(csv.DictReader(handle), None)
        if meta:
            difficulty = float(meta["difficulty"])
            source = Path(meta["source"]).stem
            if meta.get("core_sigma"):
                dumped_core_sigma = float(meta["core_sigma"])

    if args.no_dump and args.fit_sigma is not None:
        if dumped_core_sigma is None:
            sys.exit("--fit-sigma requires a dump generated with core-sigma metadata; omit --no-dump")
        if not np.isclose(args.fit_sigma, dumped_core_sigma, rtol=1e-9, atol=1e-9):
            sys.exit(
                f"existing surface dump uses core sigma {dumped_core_sigma:g} ms, "
                f"not requested {args.fit_sigma:g} ms; omit --no-dump to regenerate"
            )

    acc, miss, diffs, sigmas = load_grid()
    X, Y = np.meshgrid(diffs, sigmas)

    # For LN-focused inspection, keep the two panels with the most actionable
    # information: judgement composition and the response to alternate windows.
    figure = plt.figure(figsize=(16, 14), facecolor=PAPER)
    grid = figure.add_gridspec(2, 2, height_ratios=[1.15, 1.72], hspace=0.34, wspace=0.20)

    panel_bands(figure.add_subplot(grid[0, 0]), difficulty, args.fit_sigma)
    panel_windows(figure.add_subplot(grid[0, 1]), args.target_accuracy)
    instrument = grid[1, :].subgridspec(2, 1, height_ratios=[2.1, 1.0], hspace=0.0)
    composition_ax = figure.add_subplot(instrument[0, 0])
    balance_ax = figure.add_subplot(instrument[1, 0], sharex=composition_ax)
    panel_input_state(composition_ax)
    panel_probability_balance(balance_ax)
    composition_ax.tick_params(axis="x", labelbottom=False)
    # Treat the two axes as one continuous instrument: remove the touching
    # spines so no artificial zero-line or gap separates the panels.
    composition_ax.spines["bottom"].set_visible(False)
    balance_ax.spines["top"].set_visible(False)
    balance_ax.yaxis.grid(False)

    figure.suptitle(
        f"osu!mania hit result surface  —  {source}  —  {difficulty:.2f} stars",
        color=FG,
        fontsize=14,
        y=0.965,
    )

    args.out.parent.mkdir(parents=True, exist_ok=True)
    figure.savefig(args.out, dpi=130, facecolor=PAPER, bbox_inches="tight")
    print(f"wrote {args.out}")


if __name__ == "__main__":
    main()
