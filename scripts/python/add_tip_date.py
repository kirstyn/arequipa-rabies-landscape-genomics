#!/usr/bin/env python3
"""
annotate_beast_tree_with_dates.py

Usage:
    python3 annotate_beast_tree_with_dates.py \
        --tree analysis/BEAST_runs/Summaries/PER_RABV_2024_SS_01.mcc.tree \
        --csv tip_dates.csv \
        --idcol sample_id \
        --datecol date \
        --out annotated_tree.mcc.tree
"""

import argparse, re
import pandas as pd

def read_file_text(path):
    with open(path, "r", encoding="utf-8") as fh:
        return fh.read()

def write_file_text(path, text):
    with open(path, "w", encoding="utf-8") as fh:
        fh.write(text)

def extract_first_newick_from_nexus(text):
    # Find the first 'tree <name> = <newick>;' (case-insensitive)
    m = re.search(r'(tree\s+\S+\s*=\s*)(.+?;)', text, flags=re.IGNORECASE | re.DOTALL)
    if m:
        prefix = m.group(1)
        newick_with_semicolon = m.group(2)
        # strip the trailing semicolon for editing
        newick = newick_with_semicolon.rstrip().rstrip(';')
        return newick, (m.start(1), m.end(2))
    return None, None

def find_newick_in_text(text):
    # If NEXUS style present, extract the first tree string
    if text.lstrip().upper().startswith('#NEXUS') or 'BEGIN TREES' in text.upper():
        newick, span = extract_first_newick_from_nexus(text)
        if newick is not None:
            return newick, span, "nexus"
    # Otherwise try: whole file is a newick string (possibly with comments/annotations)
    stripped = text.strip()
    # if it ends with semicolon and contains parentheses, assume newick
    if stripped.endswith(';') and '(' in stripped:
        return stripped.rstrip(';'), (0, len(stripped)), "newick"
    # fallback: return text anyway
    return stripped, (0, len(stripped)), "newick"

def insert_annotations_safe(newick, mapping, annotate_key="date"):
    # For each tip name, find occurrences followed by :, ) , , or ; and insert the annotation
    # We'll iterate matches backwards so indices don't shift.
    for name, date in mapping.items():
        # Skip if annotation already present for this name
        if re.search(re.escape(name) + r'\s*\[&', newick):
            continue
        pat = re.compile(re.escape(name) + r'(?=[:\),;])')
        matches = list(pat.finditer(newick))
        if not matches:
            # no match found for this name
            continue
        for m in reversed(matches):
            # Ensure the match is a full label (preceding char must be start, '(' or ',' or '[')
            start = m.start()
            prev_char = newick[start-1] if start > 0 else ''
            if start == 0 or prev_char in '([,':
                insert_here = m.end()
                annotation = f'[&{annotate_key}="{date}"]'
                newick = newick[:insert_here] + annotation + newick[insert_here:]
    return newick

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--tree", required=True)
    ap.add_argument("--csv", required=True, help="CSV with tip id and date columns")
    ap.add_argument("--idcol", default="tip_name", help="column with tip names (default tip_name)")
    ap.add_argument("--datecol", default="date", help="column with date strings (default date)")
    ap.add_argument("--out", default="annotated_tree.mcc.tree")
    ap.add_argument("--annotate-key", default="date", help="annotation key to use inside [&key=\"...\"]")
    args = ap.parse_args()

    # read metadata CSV
    df = pd.read_csv(args.csv, dtype=str)
    if args.idcol not in df.columns:
        raise SystemExit(f"CSV does not contain id column '{args.idcol}'. Found columns: {list(df.columns)}")
    if args.datecol not in df.columns:
        raise SystemExit(f"CSV does not contain date column '{args.datecol}'. Found columns: {list(df.columns)}")

    mapping = dict(zip(df[args.idcol].astype(str), df[args.datecol].astype(str)))

    text = read_file_text(args.tree)
    newick, span, fmt = find_newick_in_text(text)
    if newick is None:
        raise SystemExit("Could not locate a Newick string in the tree file.")

    # insert annotations
    new_newick = insert_annotations_safe(newick, mapping, annotate_key=args.annotate_key)

    # if nexus, replace the first tree block with the annotated newick and keep rest intact
    if fmt == "nexus":
        start, end = span
        # keep prefix (like "tree NAME = ")
        prefix_match = re.search(r'(tree\s+\S+\s*=\s*)', text[start:end], flags=re.IGNORECASE)
        if prefix_match:
            prefix = prefix_match.group(1)
        else:
            # fallback: use original substring up to first non-space
            prefix = text[start:start+10]
        new_block = prefix + new_newick + ';'
        new_text = text[:start] + new_block + text[end:]
    else:
        # plain newick: write annotated newick + semicolon
        new_text = new_newick + ';'

    write_file_text(args.out, new_text)
    print(f"Annotated tree written to {args.out} (format: {fmt}). Verify tip annotations with grep '\\[&{args.annotate_key}'.")

if __name__ == "__main__":
    main()
