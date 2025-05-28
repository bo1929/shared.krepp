#!/usr/bin/env python3
import pandas as pd
import sys


def main():
    df_estimated = pd.read_csv(
        sys.argv[1],
        index_col=False,
        sep="\t",
        names=["query_id", "read_id", "ref_id", "dist_estimated", "novelty"],
    )
    df_aligned = pd.read_csv(
        sys.argv[2],
        sep="\t",
        index_col=False,
        names=["query_id", "read_id", "ref_id", "dist_aligned", "novelty"],
    )

    set_all_reads = set(df_aligned["read_id"].unique())

    set_aligned_reads = set(
        df_aligned["read_id"].loc[df_aligned["ref_id"] != "None"].unique()
    )
    set_estimated_reads = set(df_estimated["read_id"].unique())

    estimated_unmapped = set_all_reads - set_estimated_reads
    aligned_unmapped = set(
        df_aligned["read_id"].loc[df_aligned["ref_id"] == "None"].unique()
    )

    df_aligned = df_aligned.loc[df_aligned["ref_id"] != "None"]
    df_aligned = df_aligned.loc[
        df_aligned.groupby(["ref_id", "read_id"]).dist_aligned.idxmin()
    ]

    df_aligned_counts = pd.concat(
        [
            df_aligned["read_id"].value_counts(),
            pd.Series(0, index=list(aligned_unmapped)),
        ]
    ).to_frame()
    df_estimated_counts = pd.concat(
        [
            df_estimated["read_id"].value_counts(),
            pd.Series(0, index=list(estimated_unmapped)),
        ]
    ).to_frame()

    df_counts = pd.merge(
        df_aligned_counts, df_estimated_counts, left_index=True, right_index=True
    ).rename(columns={"0_x": "counts_bowtie", "0_y": "counts_krepp"})
    df_counts["novelty"] = df_estimated["novelty"].unique()[0]
    df_counts["query_id"] = df_estimated["query_id"].unique()[0]

    df_estimated_ref = (
        df_estimated.groupby(["ref_id"])
        .agg({"read_id": "size", "dist_estimated": "mean"})
        .rename(columns={"read_id": "count", "dist_estimated": "mean_dist"})
        .reset_index()
    )
    df_estimated_ref["method"] = "krepp"

    df_aligned_ref = (
        df_aligned.groupby(["ref_id"])
        .agg({"read_id": "size", "dist_aligned": "mean"})
        .rename(columns={"read_id": "count", "dist_aligned": "mean_dist"})
        .reset_index()
    )
    df_aligned_ref["method"] = "bowtie"

    df_ref = pd.concat([df_estimated_ref, df_aligned_ref])
    df_ref["ratio"] = df_ref["count"] / len(set_all_reads)
    df_ref["novelty"] = df_estimated["novelty"].unique()[0]
    df_ref["query_id"] = df_estimated["query_id"].unique()[0]

    df_ref.to_csv(
        f"{sys.argv[3]}/reference_summary-{df_estimated['query_id'].iloc[0]}.csv",
        index=False,
    )
    df_counts.to_csv(
        f"{sys.argv[3]}/count_summary-{df_estimated['query_id'].iloc[0]}.csv")

    pd.merge(
        df_estimated,
        df_aligned,
        how="outer",
        on=["novelty", "query_id", "read_id", "ref_id"],
    ).dropna().drop(columns=["novelty"]).to_csv(
        f"{sys.argv[3]}/read_summary-{df_estimated['query_id'].iloc[0]}.csv",
        index=False,
    )


if __name__ == "__main__":
    main()
