#!/usr/bin/env bash
# This script came from the mini_librispeech Kaldi example.

# Change this location to somewhere where you want to put the data.
data=./corpus/
NJ=4

# Short names for all the directories
DLL="data/local/lang"   # Work area
DLD="data/local/dict"
DT="data/train"         # Training data
DTE="data/test"
DL="data/lang"
LOC="data/local"
MDL="../data/model"     # Where the final model goes

function clock ()
{
    NOW=`date`
    echo "$1 at $NOW"
}

function section ()
{
    echo -e "\\n=== Part $1 === $2"
    clock ""
#    ls -R . > files$1.log
#    if (( $1 > 0 ))
#    then
#	old=$(($1-1))
#	diff files$old.log files$1.log > diff$1.log
#    fi
}

. ./cmd.sh
. ./path.sh

# Change this to bypass early steps
stage=0
. utils/parse_options.sh

# Make piped commands fail
set -euo pipefail

mkdir -p $data

if [ $stage -eq 0 ]; then
    section 0 "Generating Language Model data"
    AGADOR="sbcl --noinform --core $HOME/Develop/agador/agador.img"
    mkdir -p lm
    $AGADOR --lexicon lm/lexicon.txt
    $AGADOR --corpus lm/corpus.txt --count 5000
    ln -s lm data/local/lm
fi

if [ $stage -eq 1 ]; then
    section 1 "Format directories"
    # format the data as Kaldi data directories
    local/data_prep.sh $data/LibriSpeech/$part data/$

    local/prepare_dict.sh --stage 3 --nj $NJ --cmd "$train_cmd" \
	data/local/lm data/local/lm data/local/dict_nosp

    utils/prepare_lang.sh data/local/dict_nosp \
       "<UNK>" data/local/lang_tmp_nosp data/lang_nosp

    local/format_lms.sh --src-dir data/lang_nosp data/local/lm

    # Create ConstArpaLm format language model for full 3-gram and 4-gram LMs
    utils/build_const_arpa_lm.sh data/local/lm/lm_tglarge.arpa.gz \
       data/lang_nosp data/lang_nosp_test_tglarge
fi

if [ $stage -eq 2 ]; then
    section 2 "Prepare MFCCs"
    mfccdir=mfcc
    for part in dev_clean_2 train_clean_5; do
	steps/make_mfcc.sh --cmd "$train_cmd" --nj $NJ \
			   data/$part exp/make_mfcc/$part $mfccdir
	steps/compute_cmvn_stats.sh data/$part \
				    exp/make_mfcc/$part $mfccdir
    done

    # Get the shortest 500 utterances first because those are more likely
    # to have accurate alignments.
    utils/subset_data_dir.sh --shortest data/train_clean_5 \
			     500 data/train_500short
fi

# train a monophone system
if [ $stage -eq 3 ]; then
    section 3 "Train monophones"
    # TODO(galv): Is this too many jobs for a smaller dataset?
    steps/train_mono.sh --boost-silence 1.25 --nj $NJ --cmd "$train_cmd" \
			data/train_500short data/lang_nosp exp/mono
    # TODO: Understand why we use lang_nosp here...
    (
	utils/mkgraph.sh data/lang_nosp_test_tgsmall \
			 exp/mono exp/mono/graph_nosp_tgsmall
	for test in dev_clean_2; do
	    steps/decode.sh --nj $NJ --cmd "$decode_cmd" \
			    exp/mono/graph_nosp_tgsmall \
			    data/$test exp/mono/decode_nosp_tgsmall_$test
	done
    ) # &

    steps/align_si.sh --boost-silence 1.25 --nj $NJ --cmd "$train_cmd" \
		      data/train_clean_5 data/lang_nosp \
		      exp/mono exp/mono_ali_train_clean_5
fi

# train a first delta + delta-delta triphone system on all utterances
if [ $stage -eq 4 ]; then
    section 4 "Train first delta"
    steps/train_deltas.sh --boost-silence 1.25 --cmd "$train_cmd" \
			  2000 10000 data/train_clean_5 data/lang_nosp \
			  exp/mono_ali_train_clean_5 exp/tri1

    # decode using the tri1 model
    (
	utils/mkgraph.sh data/lang_nosp_test_tgsmall \
			 exp/tri1 exp/tri1/graph_nosp_tgsmall
	for test in dev_clean_2; do
      steps/decode.sh --nj $NJ --cmd "$decode_cmd" exp/tri1/graph_nosp_tgsmall \
      data/$test exp/tri1/decode_nosp_tgsmall_$test
      steps/lmrescore.sh --cmd "$decode_cmd" \
			 data/lang_nosp_test_{tgsmall,tgmed} \
			 data/$test \
			 exp/tri1/decode_nosp_{tgsmall,tgmed}_$test
      steps/lmrescore_const_arpa.sh \
        --cmd "$decode_cmd" data/lang_nosp_test_{tgsmall,tglarge} \
        data/$test exp/tri1/decode_nosp_{tgsmall,tglarge}_$test
	done
    ) # &

    steps/align_si.sh --nj $NJ --cmd "$train_cmd" \
		      data/train_clean_5 data/lang_nosp \
		      exp/tri1 exp/tri1_ali_train_clean_5
fi

# train an LDA+MLLT system.
if [ $stage -eq 5 ]; then
    section 5 "Train LDA+MLLT"
    steps/train_lda_mllt.sh --cmd "$train_cmd" \
	  --splice-opts "--left-context=3 --right-context=3" 2500 15000 \
	  data/train_clean_5 data/lang_nosp exp/tri1_ali_train_clean_5 \
	  exp/tri2b

    # decode using the LDA+MLLT model
    (
	utils/mkgraph.sh data/lang_nosp_test_tgsmall \
			 exp/tri2b exp/tri2b/graph_nosp_tgsmall
	for test in dev_clean_2; do
	    steps/decode.sh --nj $NJ --cmd "$decode_cmd" \
			    exp/tri2b/graph_nosp_tgsmall \
			    data/$test exp/tri2b/decode_nosp_tgsmall_$test
	    steps/lmrescore.sh --cmd "$decode_cmd" \
			       data/lang_nosp_test_{tgsmall,tgmed} \
			       data/$test \
			       exp/tri2b/decode_nosp_{tgsmall,tgmed}_$test
	    steps/lmrescore_const_arpa.sh \
		--cmd "$decode_cmd" \
		data/lang_nosp_test_{tgsmall,tglarge} \
		data/$test exp/tri2b/decode_nosp_{tgsmall,tglarge}_$test
	done
    ) # &

    # Align utts using the tri2b model
    steps/align_si.sh  --nj $NJ --cmd "$train_cmd" --use-graphs true \
		       data/train_clean_5 \
		       data/lang_nosp exp/tri2b exp/tri2b_ali_train_clean_5
fi

section 6 "Train tri3b"
# Train tri3b, which is LDA+MLLT+SAT
if [ $stage -eq 6 ]; then
  steps/train_sat.sh --cmd "$train_cmd" 2500 15000 \
    data/train_clean_5 data/lang_nosp exp/tri2b_ali_train_clean_5 exp/tri3b

  # decode using the tri3b model
  (
    utils/mkgraph.sh data/lang_nosp_test_tgsmall \
      exp/tri3b exp/tri3b/graph_nosp_tgsmall
    for test in dev_clean_2; do
      steps/decode_fmllr.sh --nj $NJ --cmd "$decode_cmd" \
        exp/tri3b/graph_nosp_tgsmall data/$test \
        exp/tri3b/decode_nosp_tgsmall_$test
      steps/lmrescore.sh --cmd "$decode_cmd" data/lang_nosp_test_{tgsmall,tgmed} \
        data/$test exp/tri3b/decode_nosp_{tgsmall,tgmed}_$test
      steps/lmrescore_const_arpa.sh \
        --cmd "$decode_cmd" data/lang_nosp_test_{tgsmall,tglarge} \
        data/$test exp/tri3b/decode_nosp_{tgsmall,tglarge}_$test
    done
  ) # &
fi

section 7 "compute pronabilities"
# Now we compute the pronunciation and silence probabilities
# from training data, and re-create the lang directory.
if [ $stage -eq 7 ]; then
  steps/get_prons.sh --cmd "$train_cmd" \
    data/train_clean_5 data/lang_nosp exp/tri3b
  utils/dict_dir_add_pronprobs.sh --max-normalize true \
    data/local/dict_nosp \
    exp/tri3b/pron_counts_nowb.txt exp/tri3b/sil_counts_nowb.txt \
    exp/tri3b/pron_bigram_counts_nowb.txt data/local/dict

  utils/prepare_lang.sh data/local/dict \
    "<UNK>" data/local/lang_tmp data/lang

  local/format_lms.sh --src-dir data/lang data/local/lm

  utils/build_const_arpa_lm.sh \
    data/local/lm/lm_tglarge.arpa.gz data/lang data/lang_test_tglarge

  steps/align_fmllr.sh --nj $NJ --cmd "$train_cmd" \
    data/train_clean_5 data/lang exp/tri3b exp/tri3b_ali_train_clean_5
fi

section 8 "Test tri3b"
if [ $stage -eq 8 ]; then
  # Test the tri3b system with the silprobs and pron-probs.

  # decode using the tri3b model
  utils/mkgraph.sh data/lang_test_tgsmall \
                   exp/tri3b exp/tri3b/graph_tgsmall
  for test in dev_clean_2; do
    steps/decode_fmllr.sh --nj $NJ --cmd "$decode_cmd" \
                          exp/tri3b/graph_tgsmall data/$test \
                          exp/tri3b/decode_tgsmall_$test
    steps/lmrescore.sh --cmd "$decode_cmd" data/lang_test_{tgsmall,tgmed} \
                       data/$test exp/tri3b/decode_{tgsmall,tgmed}_$test
    steps/lmrescore_const_arpa.sh \
      --cmd "$decode_cmd" data/lang_test_{tgsmall,tglarge} \
      data/$test exp/tri3b/decode_{tgsmall,tglarge}_$test
  done
fi

section 9 "Train chain model"
# Train a chain model
if [ $stage -eq 9 ]; then
  local/chain2/run_tdnn.sh
fi

# local/grammar/simple_demo.sh

# Don't finish until all background decoding jobs are finished.
wait

clock "Finished"
