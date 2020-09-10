#!/bin/bash
# Script for training a language model for toki pona.
# All audio files should have been recorded with record.sh.
source path.sh
source cmd.sh

# Short names for all the directories
DLL="data/local/lang"   # Work area
DLD="data/local/dict"
DT="data/train"         # Training data
DL="data/lang"
MDL="../data/model"     # Where the final model goes

DASH="\\n------------"
SECT="\\n======"
FILES="ls -lR data"

echo -e "$SECT Preparing data"
# Removing previously created data (from last run.sh execution)
rm -rf exp mfcc data/train/spk2utt data/train/cmvn.scp
rm -rf data/train/feats.scp data/train/split1 data/test/spk2utt
rm -rf data/test/cmvn.scp data/test/feats.scp data/test/split1
rm -rf data/local/lang data/lang data/local/tmp data/local/dict/lexiconp.txt
rm -fr $MDL
mkdir -p $DLL $DLD $DT $DL $MDL exp

$FILES > files1.log

# Prepare output directories
mkdir -p $MDL/am $MDL/graph $MDL/graph/phones $MDL/conf
# Get text references
echo -e "$DASH Fetching lexicon"
AGADOR="sbcl --noinform --core $HOME/Develop/agador/agador.img"
$AGADOR --lexicon $DLD/lexicon.txt

echo -e "$DASH Fetching text corpus"
# Get a big corpus of text to analyze.
$AGADOR --prompts $DL/corpus.txt --count 5000

echo -e "$DASH Creating speaker and phoneme references"
# Create speaker to utterance mapping
utils/fix_data_dir.sh data/train

# List all the non-silent phonemes in the lexicon.
cut -d ' ' -f 2- $DLD/lexicon.txt |  \
  sed 's/ /\n/g' | \
  grep -v -w "UNK" | grep -v -w "oov" | grep -v -w "sil" | \
  sort -u > $DLD/nonsilence_phones.txt

# List the silent ones.
( echo "sil" ; echo "UNK" ; echo "<OOV>" ) > $DLD/silence_phones.txt
echo 'sil' > $DLD/optional_silence.txt

echo -e "$DASH Prepare lang directory"
### Params  <dict-src-dir> <oov-dict-entry> <tmp-dir> <lang-dir>
utils/prepare_lang.sh $DLD "<OOV>" $DLL $DL

$FILES > files2.log
diff files1.log files2.log > diff2.log

echo -e "$SECT Process grammar"
echo -e "$DASH Analyzing corpus to make ARPA language model"

# Using the SRILM utilities
SDIR=$KALDI_ROOT/tools/srilm/bin/i686-m64
mkdir -p $DL/tmp
$SDIR/ngram-count -order $lm_order \
		  -write-vocab $DL/tmp/vocab-full.txt \
		  -wbdiscount -text $DL/corpus.txt \
		  -lm $DL/tmp/lm.arpa

echo -e "$DASH Making G.fst from ARPA model"
# G.fst is the grammar transducer.
# L.fst is the Lexicon transducer.
src/lmbin/arpa2fst --disambig-symbol=#0 \
		   --read-symbol-table=data/local/words.txt \
		   $DL/tmp/lm.arpa $DL/G.fst

#utils/format_lm_sri.sh $DL $DL/tmp/lm.arpa $DL/test

echo -e "$DASH Create graph files"
mkdir -p exp/tri1 exp/tri1/graph
utils/mkgraph.sh $DL exp/tri1 exp/tri1/graph
# Usage: utils/mkgraph.sh [options] <lang-dir> <model-dir> <graphdir>
# "e.g.: utils/mkgraph.sh data/lang_test exp/tri1/ exp/tri1/graph"

$FILES > files3.log
diff files2.log files3.log > diff3.log

echo -e "$SECT Process acoustics"
echo -e "$DASH Extracting MFCC features"
cat > conf/mfcc.conf <<EOF
--use-energy=false
--sample-frequency=16000
EOF

mfccdir=mfcc
steps/make_mfcc.sh --cmd "$train_cmd" --nj 1 \
		   $DT exp/make_mfcc/$DT $mfccdir
steps/compute_cmvn_stats.sh $DT exp/make_mfcc/$DT $mfccdir

echo -e "$DASH Monophone training"
utils/subset_data_dir.sh --first data/train 8 data/train_10k

echo -e "$DASH Triphone training and alignment"
# train_deltas.sh <#leaves> <tot-gauss> <data-dir>
#        <lang-dir> <alignment-dir> <exp-dir>
# This complains no such file exp/mono_ali/final.mdl
steps/train_deltas.sh --boost-silence 1.25 --cmd "$train_cmd" \
2000 10000 $DT $DL exp/mono_ali exp/tri1 || exit 1;

$FILES > files4.log
diff files3.log files4.log > diff4.log
