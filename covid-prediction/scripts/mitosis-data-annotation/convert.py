#!/usr/bin/env python

"""
Mitosis Data Annotation

We need to format the annotation as following to run the YOLOv4:
- Put "0" in front of the annotation if mitosis exists.
- Divided the x coordinate by 1539 and y coordinate by 1376 so that we give the coordinate in ratios.
- For Aperio x = 1539 y= 1376. For Hamamatsu x = 1663 y = 1485
- Delete the pathologist's confidence score. (These are all high confidence score)
- Add 0.05 0.05 to inform the bounding box size that goes around the mitosis.

- input "input/A07_03Dd.csv" "input/A07_03Dd.tiff"
- output "output/A07_03Dd.txt" "output/A07_03Dd.jpg"

- [x] convert tiff files to jpg
- [x] output file should be the same as image name but with .txt
"""

import csv
import os
import sys
import glob
import subprocess

def convert_csv_to_txt(input, output):
  with open(input) as infile:
    r = csv.reader(infile)
    with open(output, 'w') as outfile:
      for row in r:
        if len(row) < 3:
          continue
        w = csv.writer(outfile, delimiter=' ')
        x = float(row[0])
        y = float(row[1])
        score = float(row[2])
        mitosisExists = "0" # if score > 0.5 else "1"
        w.writerow([mitosisExists, x/1663, y/1485,0.05,0.05]) #For Aperio x = 1539 y= 1376. For Hamamatsu x = 1663 y = 1485

def convert_tiff_to_jpg(input, output):
  with subprocess.Popen(['sips', '-s', 'format', 'jpeg', input, '--out',output], stdout=subprocess.PIPE, stderr=subprocess.PIPE) as proc:
    print(proc.stdout.read())

def main():
  if len(sys.argv) < 2:
    raise Exception('You need to pass the input and output dir names as arguments')


  inputdir = sys.argv[1]
  outputdir = sys.argv[2]

  # create output dir if not exists
  if not os.path.exists(outputdir):
      os.makedirs(outputdir)

  for tiffPath in glob.glob(inputdir + '/*.tiff'):
    # get file name "path/foo.tiff" -> "foo"
    name = os.path.basename(tiffPath).replace('.tiff', '')
    csvPath = os.path.join(inputdir, name)+'.csv'
    jpgPath = os.path.join(outputdir, name)+'.jpg'
    txtPath = os.path.join(outputdir, name)+'.txt'

    print("> processing " + name)

    print("* converting tiff to jpg")
    convert_tiff_to_jpg(tiffPath, jpgPath)

    print("* converting csv to txt")
    convert_csv_to_txt(csvPath, txtPath)

main()