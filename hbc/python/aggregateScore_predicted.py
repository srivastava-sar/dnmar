#/usr/bin/python

#######################################################################################################3
# Script for evaluating aggregate Precision/Recall of distant supervision against a set of dictionaries
# / KB.  As discussed by Mintz/Hoffmann/etc... this underestimates percision because Freebase is
# incomplete.  Also there are errors due to entity linking, etc...
#######################################################################################################3

import sys
import os
import re
import math

from optparse import OptionParser

id2dict = {}
dict2id = {}

parser = OptionParser()
parser.add_option("--predictionsFile", dest='predictionsFile', default='predictions.out')
(options, args) = parser.parse_args()

def logExpSum(v):
    b = max(v)
    s = 0.0
    for x in v:
        s += math.exp(x-b)
    return math.log(s) + b

def pDist(x, dist):
    s = sum(dist.values())
    return float(dist[x]) / float(s)

i = 1
for line in open('dictionaries'):
    line = line.strip()
    id2dict[i] = line
    dict2id[line] = i
    i += 1

dictionaries = set()
for d in dict2id.keys():
    if re.search(r'^GC', d):
        continue
    for line in open('dictionary_files/%s' % d):
        entry = line.strip()
        #print "%s\t%s" % (entry, d)
        dictionaries.add("%s\t%s" % (entry, d))

tp = 0.0
fp = 0.0

sortedPredictions = []

for line in open(options.predictionsFile):
    fields = line.strip().split('\t')
    entry  = fields[0]
    nWords = int(fields[1])

    probs = {}
    for f in fields[2:]:
        (c, prob) = f.split(':')
        probs[id2dict[int(c)]] = float(prob)

    predictions = [{'c':x, 'prob':probs[x]} for x in sorted(probs.keys(), lambda a,b: cmp(probs[b], probs[a]))]
    predictions = [x for x in predictions if not re.search(r'^GC', x['c'])]

    for p in predictions:
        p['probMentioned'] = 1.0 - math.pow((1.0 - math.exp(p['prob'])), nWords)

    if len(predictions) == 0:
        continue

    #print pDist(predicted[0], dCounts)
    #for p in [predictions[0]]:
    for p in predictions:
        sortedPredictions.append({'prob':p['prob'], 'probMentioned':p['probMentioned'], 'correct':("%s\t%s" % (entry, p['c'])) in dictionaries})
        #sortedPredictions.append({'prob':p['prob'], 'correct':("%s\t%s" % (entry, p['c'])) in dictionaries})

tp = 0.0
fp = 0.0

prevPRF = None
for p in sorted(sortedPredictions, lambda a,b: cmp(b['prob'], a['prob'])):
#for p in sorted(sortedPredictions, lambda a,b: cmp(b['probMentioned'], a['probMentioned'])):
    #print p
    if p['correct']:
        #print "correct:" + str(p)
        tp += 1.0
    else:
        fp += 1.0

        fn = len(list(dictionaries)) - tp

        P = tp / (tp + fp)
        R = tp / (tp + fn)

        if P > 0 and R > 0:
            PRF = "%s\t%s\t%s" % (P,R,(2 * P * R / (P + R)))
            if prevPRF != None or prevPRF != PRF:
                print PRF
                prevPRF = PRF

#print "P = %s" % P
#print "R = %s" % R
#print "F = %s" % (2 * P * R / (P + R))
