#!/bin/bash
for file in *.zip
do
 unzip ${file}
done
for file in *.csv
do
 f="$(basename -- $file)"
 awk -F ',' '$5 != 0 {print $0}' ${file} | sed '1,11d' | head -n -8 | awk -F ',' '
    {
      for (i=1;i<=NF;++i) if ($i != "") a[i] = $i;
      if (na < NF) na = NF;
      for (i=1;i<na;++i) printf "%s,", a[i]
      printf "%s\n", a[na];
    }
    ' > ${f}_tempo.csv
done
cat *_tempo.csv > trips.csv
rm *.tempo.csv

# step 1: capture dsub output in file dsub_out.txt
dsub .... | awk 'NR == 3' | cat >> jobs.txt | awk 'NR == 3' | cat >> jobs.txt

# then: cronjob to check success & trigger new jobs
while read p; do
  dstat_out="$p"
  status="$(awk 'NR == 3' dstat_out | awk -F '\t' '{print $2;}')" 
done <jobs.txt