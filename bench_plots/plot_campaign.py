#!/usr/bin/env python3
"""Rebuild the wheat legacy-vs-modern campaign figures (RDMA finals, 2026-08-06).

To regenerate the two campaign figures from the harvested finals: this file IS
the data record (the original job-tmp copy was lost to a scratch wipe); numbers
are the solo/clean close-out values, s/step from in-log step windows.

Usage: uv run --with matplotlib --with numpy python plot_campaign.py [--linear] [--ib0]
Outputs: ripbig_scaling.png, clusters_scaling.png, speedup_vs_legacy.png,
uniform_scaling.png (next to this file); --ib0 restores the ib0-TCP arms and
the hybrid_transport figure (data kept below either way).
"""
import argparse
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np

HERE = Path(__file__).parent
RPN = 92  # ranks per node

# ---- ripbig 1136x3000 (3.408M cells), s/step ------------------------------
# single-node ladder (shared-memory, transport-free)
SN_NP = np.array([1, 2, 4, 8, 16, 32, 64, 92])
SN_MOD = np.array([6.034, 3.054, 1.575, 0.870, 0.569, 0.308, 0.180, 0.1246])
SN_LEG = np.array([7.679, 4.369, 1.902, 1.102, 0.700, 0.403, 0.230, 0.166])

# multi-node, Intel MPI mlx RDMA, solo/clean only.
# NOTE 1: 24/34/50n = decomp-probe points folded as auto-equivalent (dm24/dm34/dp50).
# NOTE 2: hybrid 46x2 @8n derived from its measured 1.40x ratio vs the slr_8 solo baseline.
# NOTE 7: legacy 1n + 2/8n + 16/30n repeats from fill fleets 326325-331/474-475
# (internal/1501, min-of-repeats; 4n repeat 0.0467 agrees +1.7%, min kept).
# Legacy impi 1n (0.1615) vs shared-mem ladder np=92 (0.166): consistent.  Legacy >=60n = RANK-SCALE COLLAPSE, not decomp: 120x46 and
# rebalanced 60x92 agree (0.0809/0.0802), 80x92 = 0.0905, 100x92 = 0.0890 — mlx ceiling
# sits between 3,680 (40n, on-trend) and 5,520 ranks; modern scales to 9,200.
RIP = {
    "mod": ([2, 4, 8, 16, 24, 30, 34, 40, 50, 60, 80, 100],
            [0.0673, 0.0413, 0.02025, 0.0120, 0.00915, 0.00768, 0.00735,
             0.00702, 0.00686, 0.00555, 0.00506, 0.00474]),
    "leg": ([1, 2, 4, 8, 16, 30, 40, 60, 80, 100],
            [0.1615, 0.0821, 0.0459, 0.0284, 0.0270, 0.0225, 0.0254, 0.0802, 0.0905, 0.0890]),
    "h2":  ([2, 4, 8, 16, 60, 100],
            [0.0717, 0.04165, 0.0284, 0.0134, 0.00735, 0.00735]),
    "h4":  ([16, 60, 100], [0.02172, 0.00702, 0.00637]),
}

# pure-MPI ib0 arms (TCP over IPoIB, pre-RDMA canonical).
# NOTE 4: modern ib0 30n fleet-load value (0.0253) replaced by the 08-06 solo heal
# (job 326333, 0.0167 — current-tip binary, not the campaign binary; on-plateau
# with 40/60n so kept); legacy ib0 keeps its >=16n collapse (real, IS the TCP story).
RIP_IB0 = {
    "mod": ([2, 4, 8, 16, 30, 40, 60],
            [0.0680, 0.0433, 0.0253, 0.0187, 0.0167, 0.01667, 0.01600]),
    "leg": ([2, 4, 8, 16, 30], [0.1026, 0.0717, 0.0607, 0.0929, 0.1133]),
}

# hybrid arms per transport (modern only), s/step.
# NOTE 5: ib0 pure cannot wire up at >=50n (TCP connect storms; practical limit
# ~60n) and ib0 23x4 cannot wire up at >=40n — those series END there, marked x.
# NOTE 6: ib0 46x2 30n (0.0307) dropped (fleet-load era); solo ceiling-hunt
# points 50/60/100n kept.  RDMA hybrid = fixed-pin (4:compact / 8:compact) solos.
HYB = {
    "ib0": {
        "pure": ([2, 4, 8, 16, 30, 40, 60], [0.0680, 0.0433, 0.0253, 0.0187, 0.0167, 0.01667, 0.01600]),
        "h2":   ([2, 16, 50, 60, 100], [0.0840, 0.0213, 0.01333, 0.01400, 0.01467]),
        "h4":   ([2, 4, 8, 16, 30], [0.1347, 0.0787, 0.0493, 0.0353, 0.0200]),
    },
    "rdma": {
        "pure": RIP["mod"],
        "h2":   ([2, 4, 8, 16, 60, 100], [0.0717, 0.04165, 0.0284, 0.0134, 0.00735, 0.00735]),
        "h4":   ([16, 60, 100], [0.02172, 0.00702, 0.00637]),
    },
}
HYB_END = {("ib0", "pure"): "wire-up fails past 60n", ("ib0", "h4"): "no wire-up ≥40n"}

# ---- cross-cluster ladders (2026-08-06 agent campaign), s/step = internal/1501
# Same ripbig deck, fixed dt, cray-mpich cxi (Slingshot) under PrgEnv-intel;
# legacy built with the node-side reorder=.false. workaround, modern = 51e682d
# reorder-proof.  NOTE 8: carpenter/barfoot 8n rode the reorder-off control
# build (mod51 8n not run; 4n control-vs-51e682d spread ~15%, single-run).
# NOTE 9: narwhal legacy 30n ran twice — 30.68 vs 171.70 s (5.6x run-to-run
# spread, congestion-collapse signature); min plotted, repeat marked.  Ruth
# legacy 16n (64x48 decomp) = 84.17 s; the 32x96 shape discriminator is still
# queued.  Cluster collapse onsets in RANKS: ruth ~3072, narwhal 3072-3840,
# wheat mlx 3680-5520 — fabric-independent.
CLUSTERS = {
    # name: (ranks/node, {code: (nodes, s/step)})
    "ruth": (192, {
        "mod": ([1, 2, 4, 8, 16], [0.05996, 0.03131, 0.01666, 0.00999, 0.00733]),
        "leg": ([1, 2, 4, 8, 16], [0.06926, 0.03502, 0.01738, 0.01102, 0.05608]),
    }),
    "narwhal": (128, {
        "mod": ([1, 2, 4, 8, 16, 30], [0.13191, 0.06462, 0.03464, 0.02065, 0.01133, 0.00933]),
        "leg": ([1, 2, 4, 8, 16, 24, 30], [0.16196, 0.07915, 0.03967, 0.02130, 0.01342, 0.01622, 0.02044]),
    }),
    "carpenter": (192, {
        "mod": ([1, 2, 4, 8], [0.05663, 0.02732, 0.01332, 0.00733]),
        "leg": ([1, 2, 4, 8], [0.06749, 0.03469, 0.01642, 0.01142]),
    }),
    "barfoot": (192, {
        "mod": ([1, 2, 4, 8], [0.05730, 0.02798, 0.01399, 0.00733]),
        "leg": ([1, 2, 4, 8], [0.06787, 0.03502, 0.01674, 0.01184]),
    }),
    # blueback (Navy Genoa 192c, SLURM, ifort-classic default) — bringup 08-07
    "blueback": (192, {
        "mod": ([1, 2, 4], [0.05863, 0.02998, 0.01599]),
        "leg": ([1, 2, 4], [0.07222, 0.03684, 0.01959]),
    }),
}
NARWHAL_LEG_30N_REPEAT = 171.70 / 1501  # the unstable second draw
CLUSTER_COLORS = {"wheat": "0.55", "ruth": "#8656c9", "narwhal": "#1baf7a",
                  "carpenter": "#eb6834", "barfoot": "#eda100",
                  "blueback": "#2a78d6"}

# ---- uniform 4096^2 (16.78M cells), s/step --------------------------------
# NOTE 3: legacy np1 anchor overlapped the 22:05 contention event -> upper bound.
UNI = {
    "mod": ([2, 4, 8, 16, 40], [0.3178, 0.1645, 0.0879, 0.0500, 0.0310]),
    "leg": ([4, 16, 40], [0.382, 0.1501, 0.1741]),
}
UNI_NP1 = {"mod": 34.44, "leg": 41.26}

STYLE = {
    "mod": dict(color="#2a78d6", marker="o", label="modern · pure MPI"),
    "leg": dict(color="#eb6834", marker="s", label="legacy (double, -O3)"),
    "h2":  dict(color="#1baf7a", marker="^", label="modern hybrid 46×2"),
    "h4":  dict(color="#eda100", marker="D", label="modern hybrid 23×4"),
}


def series_nodes(key, case):
    """Full (nodes, s/step) arrays; ripbig mod/leg get the sub-node ladder prepended."""
    n, s = map(np.asarray, (RIP if case == "rip" else UNI)[key])
    n = n.astype(float)
    if case == "rip" and key in ("mod", "leg"):
        sn = SN_MOD if key == "mod" else SN_LEG
        n = np.concatenate([SN_NP / RPN, n])
        s = np.concatenate([sn, s])
    if case == "uni":
        n = np.concatenate([[1 / RPN], n])
        s = np.concatenate([[UNI_NP1[key]], s])
    return n, s


def decorate(ax, xlab="nodes (92 ranks each)"):
    ax.grid(True, which="both", lw=0.4, color="0.88")
    ax.set_axisbelow(True)
    ax.set_xlabel(xlab)
    for sp in ("top", "right"):
        ax.spines[sp].set_visible(False)


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--linear", action="store_true", help="linear-axis variants")
    p.add_argument("--all-clusters", action="store_true",
                   help="clusters figure: every ladder incl. wheat + the "
                        "192c twins (default keeps carpenter as their proxy)")
    p.add_argument("--ib0", action="store_true",
                   help="restore the ib0-TCP arms + hybrid-transport figure "
                        "(dropped from the default set; data kept above)")
    args = p.parse_args()
    sc = "linear" if args.linear else "log"
    suf = "_linear" if args.linear else ""

    # ---------------- ripbig 3-panel ----------------
    # All panels: x = total cores; hue = code, linestyle = transport (RDMA
    # solid / ib0-TCP dashed-open).  Serial ladder rides the RDMA arms
    # (single-node = transport-free); ib0 arms start at 2 nodes.  Hybrid
    # dropped from the figure — lives in the tables + fallback rule.
    fig, (a1, a2, a3) = plt.subplots(1, 3, figsize=(15.5, 4.8))
    serial = {"mod": SN_MOD[0], "leg": SN_LEG[0]}
    CELLS = 1136 * 3000

    def arms(key):
        sn = SN_MOD if key == "mod" else SN_LEG
        n_r, s_r = map(np.asarray, RIP[key])
        cores_r = np.concatenate([SN_NP, n_r * RPN])
        s_full = np.concatenate([sn, s_r])
        n_i, s_i = map(np.asarray, RIP_IB0[key])
        return (cores_r, s_full), (n_i * RPN, np.asarray(s_i))

    for key in ("mod", "leg"):
        st = STYLE[key]
        name = "modern" if key == "mod" else "legacy"
        (cr, sr), (ci, si) = arms(key)
        solid = dict(lw=1.8, ms=4.5, color=st["color"], marker=st["marker"])
        dash = dict(lw=1.6, ms=4.5, ls="--", color=st["color"],
                    marker=st["marker"], markerfacecolor="none")
        a1.plot(cr, 1 / sr, label=f"{name} · RDMA", **solid)
        a2.plot(cr, serial[key] / sr, **solid)
        a3.plot(cr, 100 * serial[key] / sr / cr, **solid)
        if args.ib0:
            a1.plot(ci, 1 / si, label=f"{name} · ib0-TCP", **dash)
            a2.plot(ci, serial[key] / si, **dash)
            a3.plot(ci, 100 * serial[key] / si / ci, **dash)

    gx = np.array([1, 14000])
    a1.plot(gx, (1 / SN_MOD[0]) * gx, "--", color="0.65", lw=1.2,
            label="ideal (modern np1)")
    a2.plot(gx, gx, "--", color="0.65", lw=1.2)
    a3.axhline(100, ls="--", color="0.65", lw=1.2)

    cpr_ticks = [1e6, 1e5, 1e4, 1e3]
    cpr_lab = ["1M", "100k", "10k", "1k"]
    for ax, ttl, ylab in ((a1, "throughput", "steps / s"),
                          (a2, "speedup vs own serial", "speedup ×"),
                          (a3, "parallel efficiency", "% of ideal")):
        ax.set_xscale(sc)
        ax.set_xlim(0.8, 14000)
        ax.set_title(ttl, fontsize=11, pad=30)
        ax.set_ylabel(ylab)
        decorate(ax, xlab="total cores")
        top = ax.secondary_xaxis("top", functions=(lambda c: CELLS / np.maximum(c, 1e-9),
                                                   lambda p: CELLS / np.maximum(p, 1e-9)))
        top.set_xticks(cpr_ticks, cpr_lab)
        top.tick_params(labelsize=8, colors="0.45")
        top.set_xlabel("cells per core", fontsize=8, color="0.45")
        top.spines["top"].set_visible(False)
    if not args.linear:
        a1.set_yscale("log"); a2.set_yscale("log")
    a3.set_ylim(0, 105)
    a1.legend(fontsize=8, frameon=False)
    transports = "RDMA vs ib0-TCP" if args.ib0 else "RDMA"
    fig.suptitle(f"ripbig 1136×3000 — wheat, legacy vs modern, {transports} "
                 "(pure MPI, solo/clean finals, 2026-08-06)", fontsize=12)
    fig.tight_layout()
    out1 = HERE / f"ripbig_scaling{suf}.png"
    fig.savefig(out1, dpi=150)

    # ---------------- hybrid-vs-pure per transport (ib0 story — opt-in) ------
    # The fallback story: hybrid rescues scaling when RDMA is unavailable
    # (ib0 panel) and buys nothing when it is (RDMA panel).
    if args.ib0:
        fig, (h1, h2ax) = plt.subplots(1, 2, figsize=(11.5, 4.8), sharey=True)
        MIX = {"pure": ("modern · pure MPI", STYLE["mod"]),
               "h2": ("hybrid 46×2", STYLE["h2"]),
               "h4": ("hybrid 23×4", STYLE["h4"])}
        for ax, tr, ttl in ((h1, "ib0", "ib0-TCP — hybrid rescues the deep end"),
                            (h2ax, "rdma", "RDMA — hybrid not needed")):
            for mix, (name, st) in MIX.items():
                n, s = map(np.asarray, HYB[tr][mix])
                ax.plot(n * RPN, 1 / s, lw=1.8, ms=4.5, color=st["color"],
                        marker=st["marker"], label=name)
                if (tr, mix) in HYB_END:
                    ax.plot(n[-1] * RPN, 1 / s[-1], marker="x", ms=11, mew=2.2,
                            color="#d03b3b", ls="none")
                    ax.annotate(HYB_END[(tr, mix)], (n[-1] * RPN, 1 / s[-1]),
                                textcoords="offset points", xytext=(6, -11),
                                fontsize=7.5, color="#d03b3b")
            ax.plot(gx, (1 / SN_MOD[0]) * gx, "--", color="0.65", lw=1.2,
                    label="ideal (modern np1)" if tr == "ib0" else None)
            ax.set_xscale(sc)
            ax.set_xlim(120, 14000)
            ax.set_title(ttl, fontsize=11, pad=30)
            decorate(ax, xlab="total cores")
            top = ax.secondary_xaxis("top", functions=(lambda c: CELLS / np.maximum(c, 1e-9),
                                                       lambda p: CELLS / np.maximum(p, 1e-9)))
            top.set_xticks([1e4, 1e3], ["10k", "1k"])
            top.tick_params(labelsize=8, colors="0.45")
            top.set_xlabel("cells per core", fontsize=8, color="0.45")
            top.spines["top"].set_visible(False)
        if not args.linear:
            h1.set_yscale("log")
        h1.set_ylim(5, 300)
        h1.set_ylabel("steps / s")
        h1.legend(fontsize=8, frameon=False, loc="upper left")
        fig.suptitle("ripbig — hybrid MPI×OpenMP vs pure MPI, by transport", fontsize=12)
        fig.tight_layout()
        out3 = HERE / f"hybrid_transport{suf}.png"
        fig.savefig(out3, dpi=150)
        print(out3)

    # ---------------- modern-over-legacy ratio, equal transport ----------------
    # Ratio = legacy s/step / modern s/step at the SAME transport and node
    # count; the ib0 arm past 8n mostly measures legacy's TCP collapse, so the
    # RDMA arm is the quotable code-vs-code number.  Double-vs-double, -O3.
    fig, r1 = plt.subplots(figsize=(7.2, 4.8))

    def ratio(arm_l, arm_m):
        nl, sl = map(np.asarray, arm_l)
        nm, sm = map(np.asarray, arm_m)
        common = sorted(set(nl) & set(nm))
        return (np.array([c for c in common]),
                np.array([sl[list(nl).index(c)] / sm[list(nm).index(c)] for c in common]))

    r1.plot(SN_NP, SN_LEG / SN_MOD, ls=":", lw=1.8, ms=4.5, marker="o",
            color="#2a78d6", markerfacecolor="none", label="single node (transport-free)")
    n, r = ratio(RIP["leg"], RIP["mod"])
    core = n <= 40  # past 40n legacy is over its mlx rank ceiling, not comparable
    r1.plot(n[core] * RPN, r[core], lw=2.0, ms=5, marker="o", color="#2a78d6", label="RDMA")
    tail = n >= 40
    r1.plot(n[tail] * RPN, r[tail], ls=(0, (2, 3)), lw=1.6, ms=5, marker="o",
            color="#2a78d6", markerfacecolor="none")
    r1.annotate("legacy mlx rank ceiling past 3.7k ranks,\nnot code speed"
                "  (60n: 14.5x, 80n: 17.9x off-scale)",
                (42 * RPN, 6.9), fontsize=8, color="0.4", ha="right")
    if args.ib0:
        n, r = ratio(RIP_IB0["leg"], RIP_IB0["mod"])
        r1.plot(n * RPN, r, ls="--", lw=1.8, ms=5, marker="o", color="#2a78d6",
                markerfacecolor="none", label="ib0-TCP")
        r1.annotate("legacy TCP collapse,\nnot code speed", (16 * RPN, 4.97),
                    textcoords="offset points", xytext=(10, -4), fontsize=8, color="0.4")
    r1.axhline(1.0, ls="--", color="0.65", lw=1.2)
    r1.set_xscale(sc)
    r1.set_xlim(0.8, 14000)
    r1.set_ylim(0, 7.5)
    r1.set_ylabel("modern speedup over legacy ×")
    r1.set_title("modern over legacy, equal transport", fontsize=11, pad=30)
    decorate(r1, xlab="total cores")
    top = r1.secondary_xaxis("top", functions=(lambda c: CELLS / np.maximum(c, 1e-9),
                                               lambda p: CELLS / np.maximum(p, 1e-9)))
    top.set_xticks(cpr_ticks, cpr_lab)
    top.tick_params(labelsize=8, colors="0.45")
    top.set_xlabel("cells per core", fontsize=8, color="0.45")
    top.spines["top"].set_visible(False)
    r1.legend(fontsize=8, frameon=False, loc="upper left")
    fig.suptitle("ripbig — double vs double, -O3 both codes", fontsize=10, y=0.99)
    fig.tight_layout()
    out4 = HERE / f"speedup_vs_legacy{suf}.png"
    fig.savefig(out4, dpi=150)
    print(out4)

    # ---------------- cross-cluster ladders ----------------
    # Left: throughput vs total cores, one hue per cluster, modern
    # filled-solid / legacy open-dashed.  Right: the leg/mod ratio — the
    # 1.0-1.6 creep, then the collapse spikes.  Default is the DECLUTTERED
    # set: carpenter stands in for the three near-identical 192c Cray EX
    # systems (barfoot/blueback within a few %) and wheat/mlx stays in its
    # own figure; --all-clusters restores every ladder.
    fig, ((c1, c3), (c4, c2)) = plt.subplots(2, 2, figsize=(12.5, 9.2))
    ladders = {"wheat": (RPN, {"mod": RIP["mod"], "leg": RIP["leg"]})}
    ladders.update(CLUSTERS)
    if not args.all_clusters:
        ladders = {k: ladders[k] for k in ("wheat", "ruth", "narwhal", "carpenter")}
    LABELS = {"carpenter": "carpenter (≈ barfoot, blueback)",
              "wheat": "wheat (mlx RDMA)"} if not args.all_clusters else {}
    for cname, (rpn, arms_c) in ladders.items():
        col = CLUSTER_COLORS[cname]
        wheat = cname == "wheat"
        lw = 1.2 if wheat else 1.8
        for key in ("mod", "leg"):
            n, s = map(np.asarray, arms_c[key])
            if wheat and key == "leg":
                n, s = n[n <= 40], s[n <= 40]  # mlx ceiling tail told elsewhere
            kw = (dict(ls="-", marker="o", ms=3.5 if wheat else 4.5) if key == "mod"
                  else dict(ls="--", marker="s", ms=3.5 if wheat else 4.5,
                            markerfacecolor="none"))
            cpc = CELLS / (n * rpn)
            c1.plot(cpc, 1 / s, lw=lw, color=col,
                    label=LABELS.get(cname, cname) if key == "mod" else None, **kw)
            # speedup + efficiency vs the arm's own smallest run (1n; wheat
            # mod anchors at 2n — its serial story lives in ripbig_scaling)
            c3.plot(cpc, s[0] / s, lw=lw, color=col, **kw)
            c4.plot(cpc, 100.0 * (s[0] / s) / (n / n[0]), lw=lw, color=col, **kw)
        n_l, s_l = map(np.asarray, arms_c["leg"])
        n_m, s_m = map(np.asarray, arms_c["mod"])
        common = sorted(set(n_l) & set(n_m))
        rr = [s_l[list(n_l).index(c)] / s_m[list(n_m).index(c)] for c in common]
        cc = CELLS / (np.array(common) * rpn)
        if wheat:
            keep = np.array(common) <= 40
            cc, rr = cc[keep], np.array(rr)[keep]
        c2.plot(cc, rr, lw=lw, ms=4 if wheat else 5, marker="o", color=col,
                label=LABELS.get(cname, cname))
    c1.plot(CELLS / (30 * 128), 1 / NARWHAL_LEG_30N_REPEAT,
            marker="s", ms=6, color=CLUSTER_COLORS["narwhal"],
            markerfacecolor="none", ls="none")
    c1.annotate("narwhal leg 30n repeat:\n5.6× run-to-run spread",
                (CELLS / (30 * 128), 1 / NARWHAL_LEG_30N_REPEAT),
                textcoords="offset points", xytext=(8, -4), fontsize=7.5, color="0.4")
    c2.annotate("legacy rank-scale collapse\n(ruth 16n, narwhal 24-30n)",
                (CELLS / 2800, 4.0), fontsize=8, color="0.4", ha="right")
    # ideal slope -1 guide: steps/s doubles as cells/core halves; anchored on
    # the carpenter-modern 1-node point
    cpc0 = CELLS / (1 * CLUSTERS["carpenter"][0])
    gcpc = np.array([5e4, 1e2])
    c1.plot(gcpc, (1.0 / CLUSTERS["carpenter"][1]["mod"][1][0]) * (cpc0 / gcpc),
            "--", color="0.65", lw=1.2, label="ideal (linear scaling)")
    # speedup ideal vs own 1n = cpc_1n/cpc; drawn from the 192c anchor
    # (narwhal's 128c ideal is the same line shifted 1.5x leftward)
    c3.plot(gcpc, cpc0 / gcpc, "--", color="0.65", lw=1.2,
            label="ideal (192c/node anchor)")
    c4.axhline(100, ls="--", color="0.65", lw=1.2)
    c2.axhline(1.0, ls="--", color="0.65", lw=1.2)
    for ax, ttl, ylab in ((c1, "throughput", "steps / s"),
                          (c3, "parallel scaling (speedup vs own 1-node)", "speedup ×"),
                          (c4, "parallel efficiency (vs own 1-node)", "% of ideal"),
                          (c2, "legacy s/step ÷ modern s/step", "ratio ×")):
        ax.set_xscale(sc)
        ax.set_xlim(4e4, 1e2)  # inverted: scaling up -> rightward, room to 100 c/c
        ax.set_title(ttl, fontsize=11)
        ax.set_ylabel(ylab)
        decorate(ax, xlab="cells per core (scaling up →)")
    if not args.linear:
        c1.set_yscale("log")
        c3.set_yscale("log")
    c4.set_ylim(0, 115)
    c2.set_ylim(0, 6)
    c1.legend(fontsize=8, frameon=False, loc="upper left")
    c3.legend(fontsize=8, frameon=False, loc="upper left")
    fig.suptitle("ripbig across clusters — modern solid / legacy dashed; wheat mlx grey, "
                 "rest cray-mpich cxi  (2026-08-06/07)", fontsize=11)
    fig.tight_layout()
    out5 = HERE / f"clusters_scaling{suf}.png"
    fig.savefig(out5, dpi=150)
    print(out5)

    # ---------------- uniform twin ----------------
    fig, (b1, b2) = plt.subplots(1, 2, figsize=(11, 4.6))
    for key in ("mod", "leg"):
        n, s = series_nodes(key, "uni")
        b1.plot(n, 1 / s, lw=1.8, ms=4.5, **STYLE[key])
        nn, ss = map(np.asarray, UNI[key])
        b2.plot(nn, 100 * UNI_NP1[key] / ss / (nn * RPN), lw=1.8, ms=4.5, **STYLE[key])
    b1.plot(gx, (1 / UNI_NP1["mod"]) * gx * RPN, "--", color="0.65", lw=1.2,
            label="ideal (modern np1)")
    b2.axhline(100, ls="--", color="0.65", lw=1.2)
    b1.set_xscale(sc); b2.set_xscale(sc)
    if not args.linear:
        b1.set_yscale("log")
    b1.set_title("throughput", fontsize=11); b1.set_ylabel("steps / s")
    b2.set_title("parallel efficiency", fontsize=11); b2.set_ylabel("% of ideal")
    b2.set_ylim(0, 105)
    decorate(b1); decorate(b2)
    b1.legend(fontsize=8, frameon=False)
    fig.suptitle("uniform 4096² — wheat, RDMA (np1 anchors at 1/92 node)", fontsize=12)
    fig.tight_layout()
    out2 = HERE / f"uniform_scaling{suf}.png"
    fig.savefig(out2, dpi=150)

    print(out1); print(out2)


if __name__ == "__main__":
    main()
