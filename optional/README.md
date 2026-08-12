# Optional Dataset-Generation Code

This folder contains `Generate_Illustrative_Data.R`, which documents how the illustrative dataset used throughout the tutorial was created.

Running this script is **not required** to complete the analyses. The generated dataset is already supplied at:

```text
data/ergo_simple_shoulder_moment_balanced.csv
```

The generation code is included for transparency and reproducibility. The dataset is entirely simulated and does not contain observations from real research participants.

## What the script generates

The script creates a complete and balanced repeated-measures dataset containing:

- 14 simulated participants
- Male and female participant groups
- Near, Mid, and Far reach conditions
- A continuous reach-distance measurement in centimetres
- Simulated shoulder moments
- Centered and scaled reach variables used in the continuous mixed-effects model

A participant-specific random intercept is included when generating shoulder moment so that observations from the same simulated participant are correlated.

## Running the script

Open `lmm-ergonomics-guide.Rproj` in RStudio and then run `optional/Generate_Illustrative_Data.R` from the project root. No additional R packages are required.

The script uses:

```r
set.seed(1001)
```

This ensures that the same illustrative dataset is generated each time it is run with the same version of R.

## Output

The script writes the generated dataset to:

```text
data/ergo_simple_shoulder_moment_balanced.csv
```

Running it will replace the supplied CSV at that location. This should only be done when intentionally recreating the original illustrative dataset.
