#!/bin/bash
# Script for training a language model for toki pona.
# All audio files should have been recorded.
source path.sh
source cmd.sh
DL="data/local"
DLL="$DL/lang"
DASH="\\n-----------------------\\n"

# Removing previously created data (from last run.sh execution)
rm -rf exp mfcc data/train/spk2utt data/train/cmvn.scp
rm -rf data/train/feats.scp data/train/split1 data/test/spk2utt
rm -rf data/test/cmvn.scp data/test/feats.scp data/test/split1
rm -rf data/local/lang data/lang data/local/tmp data/local/dict/lexiconp.txt
mkdir -p $DL $DLL

# Get lastest lexicon.
echo -e "$DASH Fetching lexicon"
AGADOR="sbcl --noinform --core $HOME/Develop/agador/agador.img"
$AGADOR --lexicon $DLL/lexicon.txt

echo -e "$DASH Creating speaker and phoneme references"
# Create speaker to utterance mapping
utils/fix_data_dir.sh data/train

# Pull out all the phonemes.
cut -d ' ' -f 2- $DLL/lexicon.txt |  \
  sed 's/ /\n/g' | \
  grep -v -w "UNK" | grep -v -w "oov" | grep -v -w "sil" | \
  sort -u > $DLL/nonsilence_phones.txt

# List the silent ones.
( echo "sil" ; echo "UNK" ; echo "<OOV>" ) > $DLL/silence_phones.txt
echo 'sil' > $DLL/optional_silence.txt

echo -e "$DASH Prepare_lang and many checks"
### Parameters  <dict-src-dir> <oov-dict-entry> <tmp-dir> <lang-dir>
utils/prepare_lang.sh $DLL "<OOV>" data/local $DL

cat > conf/mfcc.conf <<EOF
--use-energy=false
--sample-frequency=16000
EOF

echo -e "$DASH 5.7 Extracting MFCC features"
mfccdir=mfcc
x=data/train
steps/make_mfcc.sh --cmd "$train_cmd" --nj 1 $x exp/make_mfcc/$x $mfccdir
steps/compute_cmvn_stats.sh $x exp/make_mfcc/$x $mfccdir

echo -e "$DASH 5.8 Monophone training"
utils/subset_data_dir.sh --first data/train 10 data/train_10k

echo -e "$DASH 5.9 Triphone training and alignment"
#steps/train_deltas.sh --boost-silence 1.25 --cmd "$train_cmd" \
#2000 10000 data/train data/lang exp/mono_ali exp/tri1 || exit 1;

echo -e "$DASH Counting ngrams to make lm.arpa"
# Get a big corpus of text to analyze.
$AGADOR --prompts $DL/corpus.txt --count 5000
# Using the SRILM utilities
SDIR=$KALDI_ROOT/tools/srilm/bin/i686-m64
mkdir -p $DL/tmp
$SDIR/ngram-count -order $lm_order -write-vocab \
		  $DL/tmp/vocab-full.txt \
		  -wbdiscount -text $DL/corpus.txt \
		  -lm $DL/tmp/lm.arpa

echo -e "$DASH Making G.fst ====="
lang=data/lang
mkdir -p $lang
src/lmbin/arpa2fst --disambig-symbol=#0 \
		   --read-symbol-table=data/local/words.txt \
		   $DL/tmp/lm.arpa $lang/G.fst

