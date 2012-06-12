#!/usr/bin/python

##############################################################################################3
# tweets2entityDocs.py
#
# Reads in (entity,word) pairs generated on m45 cluster, in addition to dictionaries
##############################################################################################3

import sys
import re
from optparse import OptionParser
import random

from LdaFeatures import LdaFeatures
from Vocab import Vocab
from Dictionaries import Dictionaries

parser = OptionParser()
parser.add_option("--test", action="store_true", dest="test", default=False)
parser.add_option("--noWords", action="store_false", dest="useWords", default=True)
parser.add_option("--useNoDict", action="store_true", dest="useNoDict", default=False)
parser.add_option("--maxWords", dest="maxWords", default=500)
parser.add_option("--maxEntities", dest="maxEntities", default=None)
parser.add_option("--vocab", dest="vocab", default=None)
parser.add_option("--dictDir", dest="dictDir", default='/homes/gws/aritter/twitter_nlp/data/LabeledLDA_dictionaries')
parser.add_option("--dictFile", dest="dictFile", default='dict')
(options, args) = parser.parse_args()

d2i = {}
i2d = {}
i = 1
for line in open(options.dictFile):
    d2i[line.strip()] = i
    i2d[i] = line.strip()
    i += 1

vocab = Vocab(options.vocab)
eOut = open('entities', 'w')
lOut = open('labels', 'w')
dictionaries = Dictionaries(options.dictDir, d2i)
dOut = open('dictionaries', 'w')

stop_list = set()
for word in open('/homes/gws/aritter/twitter_nlp/data/dictionaries/english.stop'):
    word = word.rstrip('\n')
    stop_list.add(word)

fNoDictOut = None
fNoDictOutLabels = None
fNoDictOutEntities = None
if not options.useNoDict:
    fNoDictOut = open('noDictOut', 'w')
    fNoDictOutLabels = open('noDictOutLabels', 'w')
    fNoDictOutEntities = open('noDictOutEntities', 'w')
    
prevEntity = None
words = []
nEntities = 0
for line in sys.stdin:
    line = line.rstrip('\n')
    (entity, word) = line.split('\t')

    ####################################################################
    # OK, we've read in all the words for this entity, now print
    # out "document"
    ####################################################################
    if prevEntity and prevEntity != entity:
        e = prevEntity
        if not options.test:
            labels = dictionaries.GetDictVector(e)
        else:
            labels = [1 for x in dictionaries.GetDictVector(e)]

        if options.maxEntities and nEntities > int(options.maxEntities):
            break
        nEntities += 1

        #Filter out stopwords, numbers and punctuation
        words = [x for x in words if (x[0] != 't') or not (x.split('=')[1] in stop_list or re.match(r'[^A-Za-z]+', x.split('=')[1]))]
        #sys.stderr.write(str(words) + "\n")

        if not options.useWords:
            words = [x for x in words if x[0] != 't']

        #limit of maxWords words per doc
        if options.maxWords > 0:
            random.shuffle(words)
            words = words[0:int(options.maxWords)]

        ###############################################################################
        # Include entities which don't appear in any dictionary?
        ###############################################################################
        if sum(labels) > 0:
        #if sum([labels[i] for i in range(len(labels)) if not re.search(r'^GC', i2d[i+1])]) > 0:
            lOut.write(' '.join([str(x) for x in labels]) + "\n")
            eOut.write("%s\n" % e)
            print ' '.join([str(vocab.GetID(x)) for x in words])
        elif options.useNoDict:
            labels = [1 for x in labels]
            lOut.write(' '.join([str(x) for x in labels]) + "\n")
            eOut.write("%s\n" % e)
            print ' '.join([str(vocab.GetID(x)) for x in words])
        else:
            #Write to file containing entities which don't appear in any dictionaries
            fNoDictOut.write(' '.join([str(vocab.GetID(x)) for x in words]) + "\n")
            labels = [1 for x in labels]
            fNoDictOutEntities.write("%s\n" % e)
            fNoDictOutLabels.write(' '.join([str(x) for x in labels]) + "\n")
            
        words = []

    words.append(word)
    prevEntity = entity
    
vocab.SaveVocab('vocab')

for d in dictionaries.dictionaries:
    dOut.write(d + "\n")
