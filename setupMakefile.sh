#!/bin/bash
NDAYS=25
NTASKS=2

# Get list of stubs
stubs=()
for ((i=1;i<=$NDAYS;i++)); do
    for ((j=1;j<=$NTASKS;j++)); do
        stub="day${i}t${j}"
        stubs+=( $stub )
    done
done

# Write Makefile
echo "ARGS=-\"package mtl\"" > Makefile
echo >> Makefile
echo "all: ${stubs[@]}" >> Makefile
echo >> Makefile

for ((i=1;i<=$NDAYS;i++)); do
    for ((j=1;j<=$NTASKS;j++)); do
        stub="day${i}t${j}"
        echo "$stub: $stub.hs util.hs" >> Makefile
        echo "	ghc \$(ARGS) $stub.hs util.hs -o $stub" >> Makefile
        echo >> Makefile
    done
done

echo ".PHONY: clean" >> Makefile
echo "clean:" >> Makefile
echo "	rm -f *.o *.hi ${stubs[@]}" >> Makefile