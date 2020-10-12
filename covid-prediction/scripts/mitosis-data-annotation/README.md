# Data converter for YoloV4 format 

## Prerequisites

* `sips` for image conversion (included in macOS)

## Run

We assume the directory passed has files formatted like in `./samples/input` with an output directory in the format of `./samples/output`

```sh
python3 convert.py <INPUT_DIRECTORY> <OUTPUT_DIRECTORY>
python3 convert.py ./samples/input /tmp/output
open /tmp/output
```
