"""Export the pause corpus as a Power BI star schema.

Reads data/final1.csv and writes powerbi/data/ as four tables plus a lookup.

Why a star schema rather than one flat table: Power BI is built for it, filters
propagate cleanly from dimensions to the fact table, and it is what the PL-300
material assumes. It also lets the linguistic hierarchy live in a dimension
with a proper sort order, which is what makes the headline finding legible.

    dim_writer              one row per writer
    dim_syntactic_context   pause position x linguistic unit, with rank
    dim_functional_context  pause position x functional role
    dim_process             process type
    fact_pause              the grain: one pause

Usage:
    python export_powerbi.py
    python export_powerbi.py --input data/final1.csv --outdir powerbi/data
    python export_powerbi.py --keep-text     # include the raw burst text
"""

from __future__ import annotations

import argparse
import re
from pathlib import Path

import pandas as pd

# Linguistic units, ordered from smallest to largest. The rank column is what
# lets a Power BI chart sort the x-axis by structure rather than alphabetically.
UNIT_RANK = {
    "word": 1,
    "NP": 2, "VP": 2, "PP": 2, "AG": 2, "CG": 2,
    "clause": 3,
    "sentence": 4,
}

UNIT_LABEL = {
    "word": "Word (within)",
    "NP": "Noun phrase", "VP": "Verb phrase", "PP": "Prepositional phrase",
    "AG": "Adverbial group", "CG": "Conjunction group",
    "clause": "Clause", "sentence": "Sentence",
}

LEVEL_LABEL = {1: "1 Word-internal", 2: "2 Phrase", 3: "3 Clause", 4: "4 Sentence"}

# Keystroke-log markers that indicate editing rather than new production.
EDIT_MARKERS = re.compile(
    r"<(BACKSPACE|DELETE|MOUSECLICK|HIGHLIGHT|REPLACED|LEFT|RIGHT|UP|DOWN)",
    re.IGNORECASE)
ANY_MARKER = re.compile(r"<[^>]*>")


def split_context(value: str) -> tuple[str, str]:
    """'end sentence' -> ('End', 'sentence'). 'NON' -> ('Not coded', 'NON')."""
    if not isinstance(value, str) or value.strip() in ("", "NON"):
        return "Not coded", "NON"
    v = value.strip()
    for prefix, label in (("mid ", "Mid"), ("end ", "End")):
        if v.lower().startswith(prefix):
            return label, v[len(prefix):].strip()
    return "Not coded", v


def read_corpus(path: Path) -> pd.DataFrame:
    """Read the CSV, trying encodings in order.

    Keystroke logs pick up whatever byte the writer actually typed, so the file
    is not reliably UTF-8. latin-1 is last because it maps all 256 byte values
    and therefore never fails -- it may render an odd character oddly, but the
    burst text is excluded from the export by default and the character counts
    are unaffected.
    """
    if not path.exists():
        raise SystemExit(
            f"\n  ERROR: {path} not found.\n"
            f"  Run 'ls data/' to see the real filename, then pass it with\n"
            f"      python export_powerbi.py --input data/YOURFILE.csv\n")

    last_error = None
    for encoding in ("utf-8", "utf-8-sig", "cp1252", "latin-1"):
        try:
            df = pd.read_csv(path, encoding=encoding)
            if encoding != "utf-8":
                print(f"  note: file is not UTF-8; read it as {encoding}")
            return df
        except UnicodeDecodeError as exc:
            last_error = exc
            continue
    raise SystemExit(f"\n  ERROR: could not decode {path}: {last_error}\n")


def build(input_path: Path, outdir: Path, keep_text: bool) -> None:
    outdir.mkdir(parents=True, exist_ok=True)
    df = read_corpus(input_path)

    expected = ["Burst", "Pause", "Syntactic.Context.of.the.pause",
                "Functional.Context.of.the.pause", "Type.of.process", "file_id"]
    missing = [c for c in expected if c not in df.columns]
    if missing:
        raise SystemExit(
            f"\n  ERROR: {input_path} is missing columns: {missing}\n"
            f"  Columns found: {list(df.columns)}\n")

    df = df.rename(columns={
        "Pause": "PauseSeconds",
        "Syntactic.Context.of.the.pause": "SyntacticRaw",
        "Functional.Context.of.the.pause": "FunctionalRaw",
        "Type.of.process": "ProcessRaw",
        "file_id": "WriterKey",
    })

    df["PauseSeconds"] = pd.to_numeric(df.PauseSeconds, errors="coerce")
    df = df[df.PauseSeconds.notna() & (df.PauseSeconds > 0)].copy()
    df["PauseMs"] = (df.PauseSeconds * 1000).round(0)

    # ---- split the two context columns into position + unit/role -----------
    syn = df.SyntacticRaw.map(split_context)
    df["SynPosition"] = [p for p, _ in syn]
    df["SynUnit"] = [u for _, u in syn]

    fun = df.FunctionalRaw.map(split_context)
    df["FunPosition"] = [p for p, _ in fun]
    df["FunRole"] = [r for _, r in fun]

    df["ProcessType"] = df.ProcessRaw.fillna("NON").replace(
        {"NON": "Not coded"}).str.strip()

    # ---- burst characteristics ---------------------------------------------
    burst = df.Burst.fillna("").astype(str)
    df["BurstChars"] = burst.str.len()
    df["BurstTextChars"] = burst.map(lambda s: len(ANY_MARKER.sub("", s)))
    df["HasEditing"] = burst.map(lambda s: bool(EDIT_MARKERS.search(s)))
    df["BurstType"] = [
        "Editing only" if edit and text == 0
        else "Mixed" if edit
        else "New text"
        for edit, text in zip(df.HasEditing, df.BurstTextChars)
    ]

    # Sequence within writer: lets a chart show how pausing changes over a session
    df = df.reset_index(drop=True)
    df["PauseSeq"] = df.groupby("WriterKey").cumcount() + 1
    df["PauseKey"] = df.index + 1

    # ---- dimensions ---------------------------------------------------------
    writers = sorted(df.WriterKey.unique())
    dim_writer = pd.DataFrame({"WriterKey": writers})
    dim_writer["Writer"] = ["Writer " + str(w).zfill(2) for w in writers]
    counts = df.groupby("WriterKey").size()
    dim_writer["PauseCount"] = dim_writer.WriterKey.map(counts)
    dim_writer.to_csv(outdir / "dim_writer.csv", index=False, encoding="utf-8-sig")

    syn_pairs = (df[["SynPosition", "SynUnit"]].drop_duplicates()
                 .sort_values(["SynUnit", "SynPosition"]).reset_index(drop=True))
    syn_pairs["SyntacticKey"] = syn_pairs.index + 1
    syn_pairs["UnitLabel"] = syn_pairs.SynUnit.map(UNIT_LABEL).fillna(syn_pairs.SynUnit)
    syn_pairs["StructuralLevel"] = syn_pairs.SynUnit.map(UNIT_RANK)
    syn_pairs["LevelLabel"] = syn_pairs.StructuralLevel.map(LEVEL_LABEL).fillna("0 Not coded")
    syn_pairs["StructuralLevel"] = syn_pairs.StructuralLevel.fillna(0).astype(int)
    syn_pairs["ContextLabel"] = (syn_pairs.SynPosition + " of " + syn_pairs.UnitLabel)
    syn_pairs.loc[syn_pairs.SynUnit == "NON", "ContextLabel"] = "Not coded"
    syn_pairs = syn_pairs[["SyntacticKey", "SynPosition", "SynUnit", "UnitLabel",
                           "ContextLabel", "StructuralLevel", "LevelLabel"]]
    syn_pairs.columns = ["SyntacticKey", "Position", "Unit", "UnitLabel",
                         "ContextLabel", "StructuralLevel", "LevelLabel"]
    syn_pairs.to_csv(outdir / "dim_syntactic_context.csv", index=False, encoding="utf-8-sig")

    fun_pairs = (df[["FunPosition", "FunRole"]].drop_duplicates()
                 .sort_values(["FunRole", "FunPosition"]).reset_index(drop=True))
    fun_pairs["FunctionalKey"] = fun_pairs.index + 1
    fun_pairs["RoleLabel"] = fun_pairs.FunRole.str.replace("_", " ").str.capitalize()
    fun_pairs["IsProcess"] = fun_pairs.FunRole.str.contains("process", case=False)
    fun_pairs["ContextLabel"] = fun_pairs.FunPosition + " of " + fun_pairs.RoleLabel
    fun_pairs.loc[fun_pairs.FunRole == "NON", "ContextLabel"] = "Not coded"
    fun_pairs = fun_pairs[["FunctionalKey", "FunPosition", "FunRole", "RoleLabel",
                           "ContextLabel", "IsProcess"]]
    fun_pairs.columns = ["FunctionalKey", "Position", "Role", "RoleLabel",
                         "ContextLabel", "IsProcess"]
    fun_pairs.to_csv(outdir / "dim_functional_context.csv", index=False, encoding="utf-8-sig")

    procs = sorted(df.ProcessType.unique())
    dim_process = pd.DataFrame({"ProcessKey": range(1, len(procs) + 1),
                                "ProcessType": procs})
    dim_process["ProcessLabel"] = dim_process.ProcessType.str.replace(
        " process", "", regex=False).str.capitalize()
    dim_process.to_csv(outdir / "dim_process.csv", index=False, encoding="utf-8-sig")

    # ---- fact table ---------------------------------------------------------
    syn_map = {(r.Position, r.Unit): r.SyntacticKey for r in syn_pairs.itertuples()}
    fun_map = {(r.Position, r.Role): r.FunctionalKey for r in fun_pairs.itertuples()}
    proc_map = dict(zip(dim_process.ProcessType, dim_process.ProcessKey))

    fact = pd.DataFrame({
        "PauseKey": df.PauseKey,
        "WriterKey": df.WriterKey,
        "SyntacticKey": [syn_map[(p, u)] for p, u in zip(df.SynPosition, df.SynUnit)],
        "FunctionalKey": [fun_map[(p, r)] for p, r in zip(df.FunPosition, df.FunRole)],
        "ProcessKey": df.ProcessType.map(proc_map),
        "PauseSeq": df.PauseSeq,
        "PauseSeconds": df.PauseSeconds.round(3),
        "PauseMs": df.PauseMs,
        "BurstChars": df.BurstChars,
        "BurstTextChars": df.BurstTextChars,
        "BurstType": df.BurstType,
        "HasEditing": df.HasEditing,
    })
    if keep_text:
        fact["BurstText"] = df.Burst

    fact.to_csv(outdir / "fact_pause.csv", index=False, encoding="utf-8-sig")

    # ---- report -------------------------------------------------------------
    print(f"\nWrote to {outdir}/")
    for f in sorted(outdir.glob("*.csv")):
        print(f"  {f.name:32} {len(pd.read_csv(f)):6} rows")

    print(f"\n  {len(fact)} pauses from {df.WriterKey.nunique()} writers")
    print(f"  Median pause: {df.PauseSeconds.median():.2f} s   "
          f"Mean: {df.PauseSeconds.mean():.2f} s   "
          f"Max: {df.PauseSeconds.max():.1f} s")

    print("\n  Mean pause by structural level (the headline finding):")
    merged = fact.merge(syn_pairs, on="SyntacticKey")
    lvl = (merged[merged.StructuralLevel > 0]
           .groupby("LevelLabel").PauseMs.agg(["mean", "size"]))
    for label, row in lvl.iterrows():
        bar = "#" * int(row["mean"] / 150)
        print(f"    {label:18} {row['mean']:7.0f} ms  (n={int(row['size']):5})  {bar}")

    print("\n  Mean pause at End vs Mid position:")
    for pos, row in merged.groupby("Position").PauseMs.agg(["mean", "size"]).iterrows():
        print(f"    {pos:12} {row['mean']:7.0f} ms  (n={int(row['size']):5})")

    if not keep_text:
        print("\n  Raw burst text excluded. Use --keep-text to include it.")
    print()


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--input", default="data/final1.csv")
    ap.add_argument("--outdir", default="powerbi/data")
    ap.add_argument("--keep-text", action="store_true",
                    help="include the raw burst text in the fact table")
    args = ap.parse_args()
    build(Path(args.input), Path(args.outdir), args.keep_text)


if __name__ == "__main__":
    main()
