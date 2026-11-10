# Separate Kobe and Majuro status figures

From the repository root, run:

```sh
Rscript extra/export-separate-status.R [output-directory]
```

The optional output directory defaults to `extra/figures`. This requires the R packages ggplot2, scales, MASS and ragg. The script uses the checked-in ensemble and Hessian data, the retained-model rule, the mixture construction, and the plotting functions in `report/augment-uncertainty-projection.R`. It exports the two current-status panels as 300 dpi PNGs, with individual, two-row legends. It does not modify the report or the original combined figure. The script identifies source sections by their boundary text and stops if those boundaries cannot be found uniquely; inspect it if the report script is reorganised.

## Separate time-dynamic panels

```sh
Rscript extra/export-separate-dynamic-status.R [output-directory]
```

This creates four PNGs: Majuro and Kobe for the diagnostic model and the 80-model median. It uses the same retained models, annual medians, common axis limits, plotting functions and year scale as the combined time-dynamic figure. Each panel includes its own title and year legend. Dependencies are ggplot2 and ragg.

## Vector PDF export

Add `pdf` after the output directory to export the six panels as separate vector PDFs:

```sh
Rscript extra/export-separate-status.R extra/figures-pdf pdf
Rscript extra/export-separate-dynamic-status.R extra/figures-pdf pdf
```

The PDFs retain vector paths and text; the dynamic colour-bar gradient may be rasterised. The current-status legend uses `PDH` and `Near-PDH` for the Hessian classifications.

For 600 dpi PNGs, use `highres` instead of `pdf` with either script. Current-status PNGs are 3240 × 3120 pixels; dynamic PNGs are 3240 × 3000 pixels.
