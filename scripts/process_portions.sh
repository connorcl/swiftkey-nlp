#! /bin/bash

for i in {1..10}; do
	Rscript process_portion.R $i;
done

