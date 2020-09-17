#!/bin/bash
## Script to record a session of toki pona audio for training
## the Kaldi recognizer.

# Parameter 1 is number of sentences to record in this session.
if [ -n "$1" ]
then
    COUNT=$1
else
    COUNT=10
fi

# Parameter 2 can restart all audio training.
if [ "$2" == "restart" ]
then
    echo "Starting over"
    rm -fr train/*
    rm -fr audio/*.wav
fi

mkdir -p data/train audio data/test

# We generate random texts from the parser grammar.
AGADOR="sbcl --noinform --core $HOME/Develop/agador/agador.img"
$AGADOR --count $COUNT --prompts /tmp/prompts.txt

RECORD="arecord -q -r 16000 -f S16_le -D pulse"
# List of all texts in each training audio
PFILE="data/train/text"
# List of all files with their IDs
WFILE="data/train/wav.scp"
# Which session each file is from
UFILE="data/train/utt2spk"
GFILE="data/train/spk2gender"
N=1

# Create recording ID from date.
IDBASE=`date +"%y%m%d%H%M"`
echo "$IDBASE m" >> $GFILE

echo -e "\n\n\nStarting recording session in 3 seconds..."
sleep 3

while read line
do
    # Create unique ID for this utterance
    IDNUM=$(printf "%02d" $N)
    ID="$IDBASE-$IDNUM"

    # Compute time to speak the line
    let "LEN=${#line}"
    let "DURATION=1+(LEN/6)"
    WNAME="audio/$ID.wav"
    echo "$ID in $DURATION:  $line"
    sleep 0.5
    $RECORD -d $DURATION $WNAME

    # Make note of this recording
    # Save the line of transcript
    echo "$ID $line" >> $PFILE
    # Save name of the audio file
    echo "$ID $WNAME" >> $WFILE
    # Save the speaker reference
    echo "$ID $IDBASE" >> $UFILE
    let "N++"
done < /tmp/prompts.txt
echo "All done recording."
