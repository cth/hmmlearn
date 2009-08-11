#!/bin/sh

CURRENT_DIR=$(pwd)
HMMLEARN_DIR="../.."

cd $HMMLEARN_DIR


HMM_INPUT="$CURRENT_DIR/sample_hmm.pl"
DATA="$CURRENT_DIR/nucleotides_1_500_primary.pl" 
CONSTRAINTS="$CURRENT_DIR/training_constraints.pl"
OUTPUT="$CURRENT_DIR/derived_hmm.pl"
CHUNKSIZE=250


./trainhmm.sh $HMM_INPUT $DATA $CONSTRAINTS $OUTPUT $CHUNKSIZE
